import MotionDetector.{Unwatch, Watch}
import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import com.pi4j.io.gpio.event.{GpioPinDigitalStateChangeEvent, GpioPinListenerDigital}
import com.pi4j.io.gpio._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MotionDetector {
  def props(controller:GpioController, pin:Pin, notify:ActorRef, threshold:Int=5) =
    Props(new MotionDetector(controller, pin, notify, threshold))

  case object Watch
  case object Unwatch
  case object Tick
  case class MotionDetected(state:Boolean, from:ActorRef)
}

/**
  * Class for motion detection.  Assumes pin will go high with motion.  Checks once a minute to see if motion was
  * triggered.  If motion triggered for a certain number of minutes, then we notify that someone is present so
  * thermostat can increase temp.  After a similar number of minutes with no activity, motion will be canceled.
  */
class MotionDetector(controller:GpioController, pin:Pin, notify:ActorRef, threshold:Int=5) extends Actor with GpioPinListenerDigital {
  private val log = Logging(context.system, this)

  protected val input:GpioPinDigitalInput = controller.provisionDigitalInputPin(pin, PinPullResistance.PULL_DOWN)

  // LED for motion
  val motionLed = context.actorOf(LedActor.props(controller, RaspiPin.GPIO_26), "motionLed")


  // Count how many motion events we get and trigger at a certain threshold.
  private var motionTicks = 0
  private var wasMotion = false

  // Tick every second.  If motion, increase ticker, if no motion, decrease.
  context.system.scheduler.schedule(1 minute, 1 minute, self, MotionDetector.Tick)

  override def receive: Receive = {
    case Watch => input.addListener(this); notify ! PinWatcher.PinNotify(input.getState.isHigh, self)
    case Unwatch => input.removeListener(this)
    case MotionDetector.Tick => {
      // If we see motion at least once a minute for (threshold) minutes, notify.  After (threshold) minutes of
      // no activity, motion has ended.
      if(wasMotion) {
        motionTicks += 1
        if(motionTicks >= threshold) {
          notify ! MotionDetector.MotionDetected(true, self)
          motionTicks = threshold
        }
        wasMotion = false
      } else {
        if(motionTicks > 0) {
          motionTicks -= 1
          if(motionTicks == 0) {
            notify ! MotionDetector.MotionDetected(false, self)
          }
        }
      }
    }
    case _ => log.warning("Received invalid message")
  }

  override def handleGpioPinDigitalStateChangeEvent(event: GpioPinDigitalStateChangeEvent): Unit = {
    wasMotion = true
    motionLed ! event.getState.isHigh
  }
}
