import Bmp180.CollectSample
import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import com.pi4j.io.i2c.{I2CBus, I2CFactory}

/**
  * The BMP180 is a pressure & temperature sensor from Bosch that connects to the I2C Bus.
  */
object Bmp180 {
  val DEFAULT_ADDRESS:Byte = 0x77.toByte

  val REG_CHIP_ID = 0xD0
  val REG_CONTROL = 0xF4
  val REG_TEMP = 0xF6
  val REG_AC1 = 0xAA
  val REG_AC2 = 0xAC
  val REG_AC3 = 0xAE
  val REG_AC4 = 0xB0
  val REG_AC5 = 0xB2
  val REG_AC6 = 0xB4
  val REG_B1 = 0xB6
  val REG_B2 = 0xB8
  val REG_MB = 0xBA
  val REG_MC = 0xBC
  val REG_MD = 0xBE

  val CHIP_ID = 0x55
  val SCO_MASK = 0x20 // Wait for this bit to clear before reading samples

  /**
    * Intiailizes a properties object for this Actor
    * @param bus the I2C bus number on the rPI for the device
    * @param address the address of the device on the I2C bus
    * @return a Props object to init the actor
    */
  def props(bus:Int, address:Byte = DEFAULT_ADDRESS):Props =  Props(new Bmp180(bus, address))

  /**
    * Send this message to the BMP180 actor once after actor creation to initialize the chip
    */
  case object Initialize

  /**
    * This actor will reply with this message when a sample is taken.
    * @param pressure atmospheric pressure in inches mercury
    * @param temperature temperature in degrees Fahrenheit
    * @param altitude current altitude above sea level in feet
    */
  case class Sample(pressure:Double, temperature:Double, altitude:Double)

  /**
    * Send the Bmp180 actor this message to start collecting a sample.
    * @param replyTo the actor to reply to with a Sample message.
    */
  case class CollectSample(replyTo:ActorRef)
}

class Bmp180(busId:Int, address:Byte) extends Actor {
  /**
    * OSS controls how many samples are taken.  In this case, we take 2^3 samples (8).  See BMP180 datasheet for
    * details, but we're not too concerned with a few uA of power consumption and a few extra tens of ms to
    * capture data.
    */
  private val oss = 3
  private val cmdStartPressure = (0x34 + (oss << 6)).toByte

  private val cmdStartTemp = 0x2E.toByte

  private val log = Logging(context.system, this)

  private val bus: I2CBus = I2CFactory.getInstance(busId)
  private val device = bus.getDevice(address)

  // Calibration parameters
  private var ac1 = 0.toShort
  private var ac2 = 0.toShort
  private var ac3 = 0.toShort
  // AC4-6 are 16-bit unsigned so keep them as Int to keep positive
  private var ac4 = 0
  private var ac5 = 0
  private var ac6 = 0
  private var b1 = 0.toShort
  private var b2 = 0.toShort
  private var mb = 0.toShort
  private var mc = 0.toShort
  private var md = 0.toShort

  // Probe to make sure it's the right chip
  private val readChipId = device.read(Bmp180.REG_CHIP_ID)
  if(readChipId != Bmp180.CHIP_ID) {
    throw new RuntimeException(f"Could not detect BMP180 expected chip ID 0x${Bmp180.CHIP_ID}%x but got $readChipId%x")
  }
  log.info(f"BMP180 detected: 0x$readChipId%x")

  /**
    * Main actor loop.
    */
  override def receive: Receive = {
    case Bmp180.Initialize => init()
    case CollectSample(replyTo:ActorRef) => collectSample(replyTo)
    case _ => log.warning("Received unknown message")
  }

  /**
    * Called when an Initialize message is received.  Reads out the calibration parameters from the chip.
    */
  def init():Unit = {
    ac1 = readShort(Bmp180.REG_AC1).toShort
    ac2 = readShort(Bmp180.REG_AC2).toShort
    ac3 = readShort(Bmp180.REG_AC3).toShort
    ac4 = readShort(Bmp180.REG_AC4)
    ac5 = readShort(Bmp180.REG_AC5)
    ac6 = readShort(Bmp180.REG_AC6)
    b1 = readShort(Bmp180.REG_B1).toShort
    b2 = readShort(Bmp180.REG_B2).toShort
    mb = readShort(Bmp180.REG_MB).toShort
    mc = readShort(Bmp180.REG_MC).toShort
    md = readShort(Bmp180.REG_MD).toShort

    log.info(f"Read calibration parameters ac1=$ac1%d ac2=$ac2%d ac3=$ac3%d ac4=$ac4%d ac5=$ac5%d " +
      f"ac6=$ac6%d b1=$b1%d b2=$b2%d mb=$mb%d mc=$mc%d md=$md%d")
  }

  /**
    * Called when a CollectSample message is received.
    * @param replyTo the Actor to reply to with a Sample message
    */
  def collectSample(replyTo:ActorRef):Unit = {
    // Read out the uncompensated values from the chip.
    val ut = readUncompensatedTemp()
    val up = readUncompensatedPressure()

    log.debug(f"Uncompensated T:$ut%d P:$up%d")

    // Math time.  See BMP180 datasheet for details.
    // Compensated temp
    var x1 = (ut - ac6) * ac5 / 32768
    var x2 = mc * 2048 / (x1 + md)
    val b5 = x1 + x2
    val t = (b5 + 8) / 16
    // Convert t (integer tenths degrees C) to degrees Fahrenheit
    val tf = t * 0.18 + 32.0

    log.debug(f"Temp calcs: x1:$x1%d x2:$x2%d b5:$b5%d t:$t%d tf:$tf%.1f")

    // Compensated pressure
    val b6 = b5 - 4000
    x1 = (b2 * (b6 * b6 / 4096)) / 2048
    x2 = ac2 * b6 / 2048
    var x3 = x1 + x2
    val b3 = (((ac1 * 4 + x3) << oss) + 2) / 4

    log.debug(f"p1 calcs: oss:$oss%d b6:$b6%d x1:$x1%d x2:$x2%d x3:$x3%d b3:$b3%d")

    x1 = ac3 * b6 / 8192
    x2 = (b1 * (b6 * b6 / 4096)) / 65536
    x3 = ((x1 + x2) + 2) / 4
    // B4 is 32-bit unsigned so use JRE Long
    val b4 = ac4 * (x3 + 32768).toLong / 32768
    // B7 is also 32-bit unsigned.
    val b7 = (up.toLong - b3) * (50000 >> oss)

    log.debug(f"p2 calcs: x1:$x1%d x2:$x2%d x3:$x3%d b4:$b4%d b7:$b7%d")

    // No, I don't have any idea what all this is doing
    var p = 0
    if(b7 < 0x80000000L) {
      p = ((b7 * 2) / b4).toInt
    } else {
      p = ((b7 / b4) * 2).toInt
    }
    x1 = (p / 256) * (p / 256)

    log.debug(f"p3 calcs: p:$p%d x1:$x1%d")

    x1 = (x1 * 3038) / 65536
    x2 = (-7357 * p) / 65536
    p = p + (x1 + x2 + 3791) / 16
    val kPa = p / 1000.0
    val inHg = kPa * 0.2953
    log.debug(f"p4 calcs: x1:$x1%d x2:$x2%d p:$p%d kPa:$kPa%.3f inHg:$inHg%.2f")

    // Pressure to altitude
    val am = 44330 * ( 1 - Math.pow(p.toDouble / 101325.0, 1.0/5.255))
    // meters to feet
    val af = am * 3.2808399
    log.debug(f"altitude $am%.1f meters ($af%.1f feet)")

    replyTo ! Bmp180.Sample(inHg, tf, af)
  }

  /**
    * Convenience function to read a short from the given address, MSB first.
    * @param regAddr the register address to start reading from
    * @return the two bytes combined into a short.  Returned as an Int so it's unsigned.
    */
  private def readShort(regAddr:Int):Int = {
    val msb = device.read(regAddr)
    val lsb = device.read(regAddr+1)

    return (msb << 8) | lsb
  }

  /**
    * Reads uncompensated pressure from the chip
    * @return the 16-to-19-bit uncompensated pressure value (depending on OSS setting)
    */
  def readUncompensatedPressure():Int = {
    // Write 0x74 to register 0xF4 to start
    device.write(Bmp180.REG_CONTROL, cmdStartPressure)

    // Loop until sample is ready.
    var ctlReg = 0
    do {
      // Wait 7.5ms
      log.debug("Sleeping for device to compute pressure")
      Thread.sleep(8L)

      // Make sure value is ready
      ctlReg = device.read(Bmp180.REG_CONTROL)
      log.debug(f"reg_ctl: 0x$ctlReg%x")

    } while((ctlReg & Bmp180.SCO_MASK) == Bmp180.SCO_MASK)

    // Read reg 0xF6, 0xF7, and 0xF8
    val msb = device.read(Bmp180.REG_TEMP)
    val lsb = device.read(Bmp180.REG_TEMP+1)
    val xlsb = device.read(Bmp180.REG_TEMP+2)

    log.debug(f"msb:$msb%d (0x$msb%x) lsb:$lsb%d (0x$lsb%x) xlsb:$xlsb%d (0x$xlsb%x)")

    // Note the BMP180 datasheet is wrong here since addition is higher precedence than shifting.
    return ((msb<<16) + (lsb<<8) + xlsb) >> (8-oss)
  }

  /**
    * Read uncompensated temperature from the chip.
    * @return the 16-bit uncompensated temperature value.
    */
  def readUncompensatedTemp():Int = {
    // Write 0x2E to register 0xF4 to start
    device.write(Bmp180.REG_CONTROL, cmdStartTemp)

    // Loop until sample is ready.
    var ctlReg = 0
    do {
      log.debug("Waiting for device to sample temp")
      // Wait 4.5ms
      Thread.sleep(5L)

      // Make sure value is ready
      ctlReg = device.read(Bmp180.REG_CONTROL)
      log.debug(f"reg_ctl: 0x$ctlReg%x")
    } while((ctlReg & Bmp180.SCO_MASK) == Bmp180.SCO_MASK)

    // Read reg 0xF6 & F7
    return readShort(Bmp180.REG_TEMP)
  }
}
