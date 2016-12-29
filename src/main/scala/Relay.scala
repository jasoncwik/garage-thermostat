import akka.actor.{Actor, Props}
import akka.event.Logging
import com.pi4j.io.gpio.{GpioController, Pin, RaspiPin}

/**
  * Actor class to simulate a relay hooked to a GPIO pin.  Send the actor a Boolean to control whether the relay is
  * sent a high or low signal.
  */
object RelayActor {
  /**
    * Create a props object to init the actor
    * @param controller the GpioController initialized in Main
    * @param pin the pin you wish to use
    * @param defaultState the default state the relay should be set to upon initialization.
    * @return a suitable Props object for Actor init.
    */
  def props(controller:GpioController, pin: Pin, defaultState:Boolean = false):Props = Props(new LedActor(controller, pin))
}

class RelayActor(controller:GpioController, pin:Pin, defaultState:Boolean) extends Actor {
  val log = Logging(context.system, this)

  private val relay = controller.provisionDigitalOutputPin(pin)
  relay.setState(defaultState)

  def receive: Receive = {
    case x:Boolean => relay.setState(x); log.info("Set relay state to " + x)
    case _      => log.info("received unknown message")
  }

}
