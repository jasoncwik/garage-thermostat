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

    val plusMatch = PatternRecognizer.PLUS_KEY.test(pulses)
    log.info(f"Plus key matches $plusMatch%.2f%%")
  }
}

class Pulse(val pulse:Long, val space:Long) {
  def matches(other:Pulse, fuzzFactor:Double): Boolean = {
    val pulseDiff = Math.abs(other.pulse - pulse).toDouble / pulse.toDouble
    if(pulseDiff > fuzzFactor) {
      Console.printf("p1: %d p2: %d diff: %.2f > %.2f\n", other.pulse, pulse, pulseDiff, fuzzFactor)
      return false
    }
    val spaceDiff = Math.abs(other.space - space).toDouble / space.toDouble
    if(spaceDiff > fuzzFactor) {
      Console.printf("s1: %d s2: %d diff: %.2f > %.2f\n", other.space, space, spaceDiff, fuzzFactor)
      return false
    }
    return true
  }
}

object PatternRecognizer {
  val SHORT_PULSE = new Pulse(600,550)
  val LONG_PULSE = new Pulse(600,1600)
  val START_PULSE = new Pulse(9000,4500)

  val PLUS_KEY = new PatternRecognizer(
    START_PULSE ::
    SHORT_PULSE ::
    SHORT_PULSE ::
      SHORT_PULSE ::
      SHORT_PULSE ::
      SHORT_PULSE ::
      SHORT_PULSE ::
      SHORT_PULSE ::
      SHORT_PULSE ::
      LONG_PULSE ::
      LONG_PULSE ::
      LONG_PULSE ::
      LONG_PULSE ::
      LONG_PULSE ::
      LONG_PULSE ::
      LONG_PULSE ::
      LONG_PULSE ::
      LONG_PULSE ::
      SHORT_PULSE ::
      LONG_PULSE ::
      SHORT_PULSE ::
      LONG_PULSE ::
      SHORT_PULSE ::
      SHORT_PULSE ::
      SHORT_PULSE ::
      SHORT_PULSE ::
      LONG_PULSE ::
      SHORT_PULSE ::
      LONG_PULSE ::
      SHORT_PULSE ::
      SHORT_PULSE ::
      SHORT_PULSE ::
      Nil, 2000L
  )

}

class PatternRecognizer(val pulses:Seq[Pulse], val max:Long) {
  val patternLength = pulses.length
  val fuzzFactor = 0.2

  def test(sample:Seq[Pulse]):Double = {
    val sampleLength = sample.length
    var fuzzyLengthMatch = false
    if(sampleLength != patternLength) {
      // Try to find "big" pulses that were combined?
      fuzzyLengthMatch = true
    }

    var matches = 0
    var j = 0
    for(i <- 0 until patternLength) {
      if (j < sampleLength) {
        val p = pulses(i)
        val s = sample(i)

        if (fuzzyLengthMatch && (s.pulse > max || s.space > max)) {
          // Probably a double entry... skip
          j += 1

        } else {
          if (p.matches(s, fuzzFactor)) {
            matches += 1
          }
        }
      }

      j += 1
    }

    return (matches * 100.0) / patternLength.toDouble
  }
}