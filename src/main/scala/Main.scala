import akka.actor.{ActorSystem, Props}
import com.pi4j.io.gpio.GpioFactory

/**
  * Created by cwikj on 12/26/2016.
  */
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")

    val system = ActorSystem("mySystem")
    val myActor = system.actorOf(Props[LedToggleActor], "led1")

    while(true) {
      myActor ! "toggle"
      Thread.sleep(100L)
    }
  }
}
