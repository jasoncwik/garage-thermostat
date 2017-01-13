import java.util.TimerTask

import IrReceiver.{Start, Stop}
import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import com.pi4j.io.gpio.event.{GpioPinDigitalStateChangeEvent, GpioPinListenerDigital}
import com.pi4j.io.gpio._

/**
  * IR Receiver / Decoder class.  Tested with VS1838B 38kHz IR Receiver chip.
  */
object IrReceiver {
  def props(controller: GpioController, pin: Pin, notify: ActorRef) = Props(new IrReceiver(controller, pin, notify))
  case object Start
  case object Stop
}

class Pulse(val pulse:Long, val space:Long)

class IrReceiver(controller:GpioController, pin:Pin, notify:ActorRef) extends Actor with GpioPinListenerDigital {
  private val log = Logging(context.system, this)

  private val END_MESSAGE = 15 // Min gap between messages in ms

  private var lastTime = 0L
  private var pulseTime = 0L
  private var spaceTime = 0L
  private val pulses = scala.collection.mutable.ListBuffer.empty[Pulse]

  // State machine
  object DecodeState extends Enumeration {
    type DecodeState = Value
    val Start, WaitPos, WaitNeg = Value
  }
  private var currentState = DecodeState.Start

  protected val input:GpioPinDigitalInput = controller.provisionDigitalInputPin(pin, PinPullResistance.PULL_UP)

  val t = new java.util.Timer()
  var task:TimerTask = new TimerTask {
    override def run():Unit = timeout()
  }

  override def receive: Receive = {
    case Start => input.addListener(this)
    case Stop => input.removeListener(this)
    case _ => log.info("received unknown message")
  }


  /**
    * Called when we timeout waiting for a negative transition.  Generally, this means end of message.
    */
  def timeout():Unit = synchronized {
    // Check state first just in case we have a race condition here.
    if(currentState == DecodeState.WaitNeg) {
      // Send the message.
      decodeMessage()
      // Wait for next message
      lastTime = 0L
      pulseTime = 0L
      spaceTime = 0L
      currentState = DecodeState.Start
      pulses.clear()
    }
  }


  override def handleGpioPinDigitalStateChangeEvent(event: GpioPinDigitalStateChangeEvent): Unit = synchronized {

    val now = System.nanoTime() / 1000  // microseconds are good enough.

    currentState match {
      case DecodeState.Start => {
        // Wait for first negative transition
        if(event.getEdge == PinEdge.FALLING) {
          lastTime = now
          currentState = DecodeState.WaitPos
        }
      }
      case DecodeState.WaitPos => {
        // Wait for positive transition
        if(event.getEdge == PinEdge.RISING) {
          // Got our positive transition.  Record pulse time
          pulseTime = now - lastTime
          lastTime = now

          // Wait for the next negative transition OR a timeout
          task = new TimerTask {
            def run():Unit = timeout()
          }
          t.schedule(task, END_MESSAGE)
          currentState = DecodeState.WaitNeg
        }
      }
      case DecodeState.WaitNeg => {
        // Wait for a negative transition
        if(event.getEdge == PinEdge.FALLING) {
          // Got the negative transition.  Record the space time.
          // don't timeout
          task.cancel()
          spaceTime = now - lastTime
          lastTime = now
          pulses += new Pulse(pulseTime, spaceTime)
          pulseTime = 0L
          spaceTime = 0L
          currentState = DecodeState.WaitPos
        }
      }
    }
  }

  def decodeMessage():Unit = {
    log.info(f"Received Message with ${pulses.length} pulses")
    for( x <- pulses) {
      log.info(f"Pulse: ${x.pulse}%dus Space: ${x.space}%dus")
    }
  }
}
