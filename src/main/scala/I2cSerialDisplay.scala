import java.nio.charset.StandardCharsets

import akka.actor.{Actor, Props}
import akka.event.Logging
import com.pi4j.io.i2c.{I2CBus, I2CFactory}

/**
  * Actor to control the I2C-connected 16x2 serial display.  Adapted from OSOYOO's pyython code sample at:
  * http://osoyoo.com/?p=1031
  */
object I2cSerialDisplay {
  /**
    * Creates params to init an I2cSerialDisplay actor.
    * @param bus the I2C bus continaing the controller chip.  For Model B boards, this is I2CBus.BUS_1
    * @param address the address of the controller chip on the bus.  This is usually 0x27 or 0x3f
    * @return the props object
    */
  def props(bus:Int, address:Byte):Props = Props(new I2cSerialDisplay(bus, address))

  val LCD_WIDTH = 16 // Maximum characters per line

  // Define some device constants
  val LCD_CHR = 1.toByte // Mode - Sending data
  val LCD_CMD = 0.toByte // Mode - Sending command

  val LCD_LINE_1:Byte = 0x80.toByte // LCD RAM address for the 1st line
  val LCD_LINE_2:Byte = 0xC0.toByte // LCD RAM address for the 2nd line
  val LCD_LINE_3:Byte = 0x94.toByte // LCD RAM address for the 3rd line
  val LCD_LINE_4:Byte = 0xD4.toByte // LCD RAM address for the 4th line

  val LCD_BACKLIGHT_ON  = 0x08.toByte // On
  val LCD_BACKLIGHT_OFF = 0x00.toByte // Off

  val ENABLE = 0x04.toByte // Enable bit

  // Timing constants
  val E_PULSE = 1L
  val E_DELAY = 1L

  // Messages
  case class Message(s:String, line:Byte)
  case object ClearDisplay
}

class I2cSerialDisplay(busId:Int, address:Byte) extends Actor {
  val log = Logging(context.system, this)
  val bus: I2CBus = I2CFactory.getInstance(busId)
  val device = bus.getDevice(address)

  //  Init the display
  lcd_init()

  def lcd_init():Unit = {
    // Initialise display
    lcd_byte(0x33.toByte, I2cSerialDisplay.LCD_CMD) // 110011 Initialise
    lcd_byte(0x32.toByte, I2cSerialDisplay.LCD_CMD) // 110010 Initialise
    lcd_byte(0x06.toByte, I2cSerialDisplay.LCD_CMD) // 000110 Cursor move direction
    lcd_byte(0x0C.toByte, I2cSerialDisplay.LCD_CMD) // 001100 Display On,Cursor Off, Blink Off
    lcd_byte(0x28.toByte, I2cSerialDisplay.LCD_CMD) // 101000 Data length, number of lines, font size
    lcd_byte(0x01.toByte, I2cSerialDisplay.LCD_CMD) // 000001 Clear display
    Thread.sleep(I2cSerialDisplay.E_DELAY)
  }
    

  def lcd_byte(bits:Byte, mode:Byte):Unit = {
    // Send byte to data pins
    // bits = the data
    // mode = 1 for data
    //        0 for command

    val bits_high:Byte = (mode | (bits & 0xF0.toByte) | I2cSerialDisplay.LCD_BACKLIGHT_ON).toByte
    val bits_low:Byte = (mode | ((bits << 4) & 0xF0.toByte) | I2cSerialDisplay.LCD_BACKLIGHT_ON).toByte

    // High bits
    device.write(bits_high)
    lcd_toggle_enable(bits_high)

    // Low bits
    device.write(bits_low)
    lcd_toggle_enable(bits_low)
  }

  def lcd_toggle_enable(bits:Byte):Unit = {
    // Toggle enable
    Thread.sleep(I2cSerialDisplay.E_DELAY)
    device.write((bits | I2cSerialDisplay.ENABLE).toByte)
    Thread.sleep(I2cSerialDisplay.E_PULSE)
    device.write((bits & ~I2cSerialDisplay.ENABLE).toByte)
    Thread.sleep(I2cSerialDisplay.E_DELAY)
  }

  def lcd_string(message:String, line:Byte):Unit = {
    // Send string to display
    var text = message
    if(message.length > I2cSerialDisplay.LCD_WIDTH) {
      log.warning("Truncating string: " + message);
      text = message.substring(0, I2cSerialDisplay.LCD_WIDTH)
    }

    // Pad out to width to overwrite any existing text
    text = text.padTo(I2cSerialDisplay.LCD_WIDTH, ' ')

    lcd_byte(line, I2cSerialDisplay.LCD_CMD)

    // Convert to ASCII
    val bytes = text.getBytes(StandardCharsets.US_ASCII)

    bytes.foreach( f => {
      //log.info("Sending serial: " + f)
      lcd_byte(f, I2cSerialDisplay.LCD_CHR)
    } )
  }

  override def receive: Receive = {
    case I2cSerialDisplay.ClearDisplay => lcd_init()
    case I2cSerialDisplay.Message(s, line) => lcd_string(s, line)
    case _      => log.info("received unknown message")
  }
}
