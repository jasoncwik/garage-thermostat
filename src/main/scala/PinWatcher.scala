import PinWatcher.{Poll, Unwatch, Watch}
import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import com.pi4j.io.gpio.event.{GpioPinDigitalStateChangeEvent, GpioPinListenerDigital}
import com.pi4j.io.gpio.{GpioController, GpioPinDigitalInput, Pin, PinPullResistance}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global



object PinWatcher {
  def props(controller:GpioController, pin:Pin, notify:ActorRef) = Props(new PinWatcher(controller, pin, notify))

  case object Watch
  case object Unwatch
  case object Poll
  case class PinNotify(state:Boolean, from:ActorRef)
}

/**
  * Created by cwikj on 12/28/2016.
  */
class PinWatcher(controller:GpioController, pin:Pin, notify:ActorRef) extends Actor with GpioPinListenerDigital {
  private val log = Logging(context.system, this)

  protected val input:GpioPinDigitalInput = controller.provisionDigitalInputPin(pin, PinPullResistance.PULL_DOWN)

  // Once a minute, poll the actual pin state.  I've seen some glitches and this should reset them.
  context.system.scheduler.schedule(1 minute, 1 minute, self, Poll)

  override def receive: Receive = {
    case Watch => input.addListener(this); notify ! PinWatcher.PinNotify(input.getState.isHigh, self)
    case Unwatch => input.removeListener(this)
    case Poll => notify ! PinWatcher.PinNotify(input.isHigh, self)
    case _ => log.info("received unknown message")
  }

  override def handleGpioPinDigitalStateChangeEvent(event: GpioPinDigitalStateChangeEvent): Unit = {
    log.info(String.format("Pin %s changed state: %s", input.getPin, event.getState))
    notify ! PinWatcher.PinNotify(event.getState.isHigh, self)
  }
}
