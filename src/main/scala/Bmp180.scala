import akka.actor.Actor.Receive
import akka.actor.{Actor, Props}
import akka.event.Logging
import com.pi4j.io.i2c.{I2CFactory, I2CBus}

/**
  * The BMP180 is a pressure & temperature sensor from Bosch that connects to the I2C Bus.
  */
object Bmp180 {
  val DEFAULT_ADDRESS = 0x77.toByte

  val REG_CHIP_ID = 0xD0
  val REG_CONTROL = 0xF4

  val CHIP_ID = 0x55
  val MODE_STANDARD = 0x40 // 2x oversampling for pressure
  

  /**
    * Intiailizes a properties object for this Actor
    * @param bus the I2C bus number on the rPI for the device
    * @param address the address of the device on the I2C bus
    * @return a Props object to init the actor
    */
  def props(bus:Int, address:Byte = DEFAULT_ADDRESS):Props =  Props(new Bmp180(bus, address))

  case object Initialize
  case class Sample(pressure:Float, temperature:Float, altitude:Float)
}

class Bmp180(busId:Int, address:Byte) extends Actor {
  val log = Logging(context.system, this)

  val bus: I2CBus = I2CFactory.getInstance(busId)
  val device = bus.getDevice(address)

  // Calibration parameters
  var ac1 = 0
  var ac2 = 0
  var ac3 = 0
  var ac4 = 0
  var ac5 = 0
  var ac6 = 0
  var b1 = 0
  var b2 = 0
  var mb = 0
  var mc = 0
  var md = 0

  // Probe to make sure it's the right chip
  val readChipId = device.read(Bmp180.REG_CHIP_ID)
  if(readChipId != Bmp180.CHIP_ID) {
    throw new RuntimeException(f"Could not detect BMP180 expected chip ID 0x${Bmp180.CHIP_ID}%x but got $readChipId%x")
  }
  log.info(f"BMP180 detected: 0x$readChipId%x")

  // Configure for standard mode.


  override def receive: Receive = {
    case Bmp180.Initialize => init()
    case _ => log.warning("Received unknown message")
  }

  /**
    * Call this first.  Reads out the calibration parameters from the chip.
    */
  def init():Unit = {

  }

  def readShort(regAddr:Int):Int = {
    val msb = device.read(regAddr)
    val lsb = device.read(regAddr+1)

    return (msb << 8) | lsb
  }
}
