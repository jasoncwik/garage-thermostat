import akka.actor.{Props, ActorRef}
import akka.event.Logging
import com.pi4j.io.gpio.event.{GpioPinDigitalStateChangeEvent, GpioPinListenerDigital}
import com.pi4j.io.gpio.{GpioController, Pin}

/**
  * IR Receiver / Decoder class.  Tested with VS1838B 38kHz IR Receiver chip.
  */
object IrReceiver {
  def props(controller: GpioController, pin: Pin, notify: ActorRef) = Props(new IrReceiver(controller, pin, notify))
}

class IrReceiver(controller:GpioController, pin:Pin, notify:ActorRef) extends PinWatcher(controller, pin, notify)
    with GpioPinListenerDigital {

  private val log = Logging(context.system, this)

  override def handleGpioPinDigitalStateChangeEvent(event: GpioPinDigitalStateChangeEvent): Unit = {
    log.info(String.format("Pin %s changed state: %s", input.getPin, event.getState))

  }
}
