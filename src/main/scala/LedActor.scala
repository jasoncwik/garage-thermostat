import LedActor.Toggle
import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
import com.pi4j.io.gpio._

/**
  * Simple actor that can be used to control an LED hooked up to a GPIO pin.
  */
object LedActor {
  /**
    * Create a props object to init the actor
    * @param controller the GpioController initialized in Main
    * @param pin the pin you wish to use
    * @return a suitable Props object for Actor init.
    */
  def props(controller:GpioController, pin: Pin):Props = Props(new LedActor(controller, pin))

  /**
    * pass this message to toggle the state of the pin.  You can also pass true and false to explicitly set the
    * state of the pin.
    */
  case object Toggle

}


class LedActor(controller: GpioController, pin: Pin) extends Actor {
  val log = Logging(context.system, this)

  // Init LED pin and set default state.
  var ledState = false
  private val led = controller.provisionDigitalOutputPin(pin)
  led.setState(ledState)

  def receive: Receive = {
    case x:Boolean => ledState = x; led.setState(x); log.debug("Set pin state to " + ledState)
    case Toggle => ledState = !ledState; led.setState(ledState); //log.info("Toggled pin state.  New state " + ledState)
    case _      => log.info("received unknown message")
  }

}
