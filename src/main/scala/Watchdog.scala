import Watchdog.{Disable, Enable, Ping}
import akka.actor.{Actor, Cancellable}
import akka.event.Logging
import com.pi4j.io.wdt.impl.WDTimerImpl

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


object Watchdog {
  case class Enable(seconds:Int)
  case object Disable
  case object Ping
}

/**
  * Watchdog timer to reboot rPI in case it hangs or the application crashes.
  */
class Watchdog extends Actor {
  private val log = Logging(context.system, this)
  val wDTimer = WDTimerImpl.getInstance
  var cancelable:Option[Cancellable] = None

  override def receive: Receive = {


    case Enable(seconds:Int) => {
      wDTimer.open()
      log.info("GetTimeOut: " + wDTimer.getTimeOut)
      wDTimer.setTimeOut(seconds)
      log.info("GetTimeOut: " + wDTimer.getTimeOut)

      // Start up a pinger
      val interval = seconds/2
      cancelable = Some(context.system.scheduler.schedule(interval seconds, interval seconds, self, Ping))

      // Shutdown hook to stop the timer
      java.lang.Runtime.getRuntime.addShutdownHook(
        new Thread() {
          override def run():Unit = {
            wDTimer.disable()
            wDTimer.close()
          }
        }
      )
    }

    case Disable => {
      wDTimer.disable()
      cancelable.foreach(f => {f.cancel()})
      cancelable = None
      wDTimer.close()
    }

    case Ping => {
      if(cancelable.isDefined) wDTimer.heartbeat()
    }

    case _ => {
      log.warning("Unknown message received")
    }

  }
}
