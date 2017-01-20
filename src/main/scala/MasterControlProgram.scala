import java.text.{DateFormat, SimpleDateFormat}
import java.util.{Calendar, Date, Locale, TimeZone}

import MasterControlProgram.{Update, UpdateDisplay}
import akka.actor.{Actor, Props}
import akka.event.Logging
import com.pi4j.io.gpio.{GpioFactory, PinState, RaspiPin}
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
  case object UpdateDisplay
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
  val heatCall = context.actorOf(RelayActor.props(controller, RaspiPin.GPIO_04, PinState.LOW), "heatCall")

  // Watch for the door open/close
  val doorSensor = context.actorOf(PinWatcher.props(controller, RaspiPin.GPIO_05, self), "doorSensor")
  doorSensor ! PinWatcher.Watch

  // Temp sensor
  val tempPressSensor = context.actorOf(Bmp180.props(I2CBus.BUS_1), "bmp180")
  tempPressSensor ! Bmp180.Initialize
  context.system.scheduler.schedule(5 seconds, 5 seconds, tempPressSensor, Bmp180.CollectSample(self))

  // Buttons for heat up/down
  val tempUp = context.actorOf(PinWatcher.props(controller, RaspiPin.GPIO_02, self), "btnTempUp")
  tempUp ! PinWatcher.Watch
  val tempDown = context.actorOf(PinWatcher.props(controller, RaspiPin.GPIO_03, self), "btnTempDown")
  tempDown ! PinWatcher.Watch

  // Motion sensor
  val motionDetector = context.actorOf(MotionDetector.props(controller, RaspiPin.GPIO_27, self), "motionDetect")
  motionDetector ! MotionDetector.Watch


  // IR receiver for temperature control
  val irReceiver = context.actorOf(IrReceiver.props(controller, RaspiPin.GPIO_00, self), "irReceiver")
  irReceiver ! IrReceiver.Start

  // Watchdog timer in case system hangs
  val wdTimer = context.actorOf(Props[Watchdog], "watchdog")
  wdTimer ! Watchdog.Enable(15)

  // Local state for heating logic
  var doorOpen = false

  // Is someone present?
  var motionDetect = false

  // If heat call is on
  var heating = false

  // Temperature set point for heating (deg F)
  var setpoint = 50.0

  var currentTemp = 50.0

  // Maximum difference for heating (stop at setpoint + maxDifference)
  // and don't kick in until setpoint - maxDifference (deg F)
  var maxDifference = 1.0

  // Maximum time to run after setpoint (in ms)
  var maxTime = 60000L

  // Time we reached setpoint
  var setpointTime = 0L

  // Ping ourselves every second to evaluate heating logic
  context.system.scheduler.schedule(5 seconds, 1 second, self, Update)

  // Otherwise, update display every 5 seconds.
  context.system.scheduler.schedule(5 seconds, 5 seconds, self, UpdateDisplay)

  // Shutdown hook to stop the timer
  java.lang.Runtime.getRuntime.addShutdownHook(
    new Thread() {
      override def run():Unit = {
        GpioFactory.getInstance().shutdown()
        log.info("GPIO Shutdown")
      }
    }
  )

  def doUpdate():Unit = {
    // Evaluate heat call
    if(heating) {
      // turn off?
      if(doorOpen) {
        heating = false
        heatCall ! heating
        log.info("Turn heat off because door is open")
        updateDisplay()
      } else if(currentTemp > (setpoint+maxDifference)) {
        heating = false
        heatCall ! heating
        log.info("Turn heat off because setpoint exceeded")
        updateDisplay()
      }
    } else {
      // turn on?
      if(currentTemp < (setpoint-maxDifference) && !doorOpen) {
        heating = true
        heatCall ! heating
        log.info("Turn heat on")
        updateDisplay()
      }
    }

  }

  override def receive: Receive = {
    case PinWatcher.PinNotify(state, from) => {
      if(doorSensor.eq(from)) {
        doorOpen = !state
        log.info("Garage door is open? " + doorOpen)
        updateDisplay()
      } else if(tempUp.equals(from) && state == true) { // Only trigger on rising edge
        setpoint += 1.0
        updateDisplay()
      } else if(tempDown.equals(from) && state == true) {
        setpoint -= 1.0
        updateDisplay()
      }
    }
    case Bmp180.Sample(_, temperature, _) => currentTemp = temperature
    case Update => doUpdate()
    case UpdateDisplay => updateDisplay()
    case MotionDetector.MotionDetected(state, _) => motionDetect = state
    case _      => log.info("received unknown message")
  }

  private def updateDisplay():Unit = {
    // 0123456789ABCDEF
    // hh:mma Temp:XXºF
    // Set:XXºF YYYYYYY    YYYY = HEAT ON or DOOR UP

    val df = new SimpleDateFormat("hh:mma")
    val time = df.format(new Date()).stripSuffix("M") // just want A/P not AM/PM
    val deg = "\u00DF"
    val status = if(doorOpen) "DOOR UP" else if(heating) "HEAT ON" else "IDLE"
    val line1 = f"$time%s Temp:$currentTemp%.0f$deg%sF"
    val line2 = f"Set:$setpoint%.0f$deg%sF $status%s"

    log.info(line1)
    log.info(line2)

    lcdDisplay ! I2cSerialDisplay.Message(line1, I2cSerialDisplay.LCD_LINE_1)
    lcdDisplay ! I2cSerialDisplay.Message(line2, I2cSerialDisplay.LCD_LINE_2)
  }
}
