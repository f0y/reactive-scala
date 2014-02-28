import akka.actor.ActorRef
import akka.pattern.AskTimeoutException
import akka.util.Timeout
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global

package object kvstore {

  def log(x: Any)(implicit sender : akka.actor.ActorRef)  = {
    print(x)
    println(s" from ${sender.path.name}")
  }

}