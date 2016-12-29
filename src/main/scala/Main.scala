import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

/**
  * Entrypoint.  Init actors and startup system.
  */
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")

    val config = ConfigFactory.load()

    // In test mode, we will simulate everything in absence of GPIO hardware on the rPI
    var test = false;
    if(args.length > 0 && "test".equals(args(0))) {
      test = true;
    }

    val system = ActorSystem("mySystem", config)

    val mcp = system.actorOf(MasterControlProgram.props(), "MCP")

    while(true) {

      Thread.sleep(100L)
    }
  }
}
