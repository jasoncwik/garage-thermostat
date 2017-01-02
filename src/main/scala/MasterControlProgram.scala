import java.text.{DateFormat, SimpleDateFormat}
import java.util.{Calendar, Date, Locale, TimeZone}

import MasterControlProgram.Update
import akka.actor.{Actor, Props}
import akka.event.Logging
import com.pi4j.io.gpio.{GpioFactory, RaspiPin}
import com.pi4j.io.i2c.I2CBus

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Main actor to control thermostat functions.
  *
  * If you don't get the reference, you're a bad Program.
  */
object MasterControlProgram {
  def props():Props = Props(new MasterControlProgram())

  case object Update
}

class MasterControlProgram extends Actor {
  val log = Logging(context.system, this)

  val controller = GpioFactory.getInstance()

  //
  // Create children
  //

  // First, an LED to show that the app is running
  val heartbeatLED = context.actorOf(LedActor.props(controller, RaspiPin.GPIO_06), "led1")
  // Toggle the LED every second while the app is running
  context.system.scheduler.schedule(0 seconds, 1 second, heartbeatLED, LedActor.Toggle)

  // Create the display
  val lcdDisplay = context.actorOf(I2cSerialDisplay.props(I2CBus.BUS_1, 0x3f.toByte), "lcdDisplay")
  log.info("Initializing LCD display")
  lcdDisplay ! I2cSerialDisplay.Message("Initializing...", I2cSerialDisplay.LCD_LINE_1)

  // Create a relay to control the heat call
  val heatCall = context.actorOf(RelayActor.props(controller, RaspiPin.GPIO_04, false), "heatCall")

  // Watch for the door open/close
  val doorSensor = context.actorOf(PinWatcher.props(controller, RaspiPin.GPIO_05, self), "doorSensor")
  doorSensor ! PinWatcher.Watch

  // Temp sensor
  val tempPressSensor = context.actorOf(Bmp180.props(I2CBus.BUS_1), "bmp180")

  // Motion sensor

  // IR sensor for temperature control

  // Local state for heating logic
  var doorOpen = false

  // Temperature set point for heating (deg F)
  var setpoint = 50.0

  // Maximum difference for heating (stop at setpoint + maxDifference)
  // and don't kick in until setpoint - maxDifference (deg F)
  var maxDifference = 1.0

  // Maximum time to run after setpoint (in ms)
  var maxTime = 60000L

  // Time we reached setpoint
  var setpointTime = 0L

  // Ping ourselves every 5 seconds to evaluate heating logic and to update displays
  context.system.scheduler.schedule(5 seconds, 1 second, self, Update)

  def doUpdate():Unit = {
    // Evaluate heat call

    // Update display
    val cal = Calendar.getInstance(TimeZone.getDefault)
    val df = DateFormat.getTimeInstance(DateFormat.MEDIUM)

    lcdDisplay ! I2cSerialDisplay.Message(df.format(cal.getTime), I2cSerialDisplay.LCD_LINE_1)
    if(doorOpen) {
      lcdDisplay ! I2cSerialDisplay.Message("Door Open", I2cSerialDisplay.LCD_LINE_2)
    } else {
      lcdDisplay ! I2cSerialDisplay.Message("Door Closed", I2cSerialDisplay.LCD_LINE_2)
    }

    // Toggle heatCall every 10s for now.
    val heatState = ((System.currentTimeMillis() / 6000L) % 2) == 0
    heatCall ! heatState
  }

  override def receive: Receive = {
    case PinWatcher.PinNotify(state, from) => {
      if(doorSensor.eq(from)) {
        doorOpen = !state;
        log.info("Garage door is open? " + doorOpen)

      }
    }
    case Update => doUpdate
    case _      => log.info("received unknown message")
  }
}
