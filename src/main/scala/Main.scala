import akka.actor.ActorSystem
import com.pi4j.io.gpio.{GpioFactory, RaspiPin}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Entrypoint.  Init actors and startup system.
  */
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")

    val controller = GpioFactory.getInstance()
    val system = ActorSystem("mySystem")
    val heartbeatLED = system.actorOf(LedActor.props(controller, RaspiPin.GPIO_06), "led1")

    // Toggle the LED every second while the app is running
    system.scheduler.schedule(0 seconds, 1 second, heartbeatLED, LedActor.Toggle)
    while(true) {

      Thread.sleep(100L)
    }
  }
}
