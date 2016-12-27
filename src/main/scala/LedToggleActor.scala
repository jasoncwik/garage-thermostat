import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
import com.pi4j.io.gpio.{RaspiPin, GpioController, GpioPinDigitalOutput, GpioFactory}

/**
  * Created by cwikj on 12/26/2016.
  */
class LedToggleActor extends Actor {
  val log = Logging(context.system, this)

  var ledState = false;

  var gpio = GpioFactory.getInstance();
  var led = gpio.provisionDigitalOutputPin(RaspiPin.GPIO_06);
  led.setState(ledState)

  def receive = {
    case "test" => log.info("You got test!  Current state is " + ledState)
    case "toggle" => ledState = !ledState; led.setState(ledState); log.info("Toggled pin state.  New state " + ledState)
    case "shutdown" => gpio.shutdown();
    case _      => log.info("received unknown message")
  }

}
