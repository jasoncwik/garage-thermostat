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

class Pulse(val high:Long, val low:Long)

class IrReceiver(controller:GpioController, pin:Pin, notify:ActorRef) extends PinWatcher(controller, pin, notify)
    with GpioPinListenerDigital {

  private val END_MESSAGE = 65000 // 65ms
  private val DISCARD = 500000 // 500ms -- discard current state and start over

  private val pulses = scala.collection.mutable.ListBuffer.empty[Pulse]

  private val log = Logging(context.system, this)

  private var lastState = true
  private var lastTime = 0L
  private var highTime = 0L
  private var lowTime = 0L

  override def handleGpioPinDigitalStateChangeEvent(event: GpioPinDigitalStateChangeEvent): Unit = {
    val now = System.nanoTime()

    // Length since last message in microseconds
    val duration = (now - lastTime) / 1000
    lastTime = now
    if(duration > DISCARD) {
      // Way too long... start over
      pulses.clear()
      lastState = event.getState.isHigh
      highTime = 0L
      lowTime = 0L
    } else if(duration > END_MESSAGE) {
      // Message restarting...
      if(pulses.length == 0) {
        // ??
        log.warning("no pulses received?")
        // same as discard
        lastState = event.getState.isHigh
        highTime = 0L
        lowTime = 0L
      } else {
        // End of message
        decodeMessage()
      }
    } else {
      // Normal pulse
      if(event.getState.isHigh) {
        highTime += duration
      } else {
        lowTime += duration
      }
      if(highTime > 0L && lowTime > 0L) {
        // Full pulse
        pulses += new Pulse(highTime, lowTime)
        highTime = 0L
        lowTime = 0L
      }
      lastState = event.getState.isHigh
    }

  }

  def decodeMessage():Unit = {
    log.info(f"Received Message with ${pulses.length} pulses")
    for( x <- pulses) {
      log.info(f"High: ${x.high}%dus Low: ${x.low}%dus")
    }
  }
}
