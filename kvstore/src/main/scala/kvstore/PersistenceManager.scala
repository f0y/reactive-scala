package kvstore

import akka.actor._
import scala.concurrent.duration.{FiniteDuration, Duration}
import kvstore.PersistenceManager.TryPersist
import kvstore.Persistence.{PersistenceException, Persist, Persisted}


object PersistenceManager {
  def props(persistenceProps: Props,
            timeout: FiniteDuration,
            retryDelay: FiniteDuration,
            onSuccess: Persisted => Unit): Props =
    Props(new PersistenceManager(persistenceProps, timeout, retryDelay, onSuccess))

  case class TryPersist(key: String, valueOption: Option[String], id: Long)
}


class PersistenceManager(val persistenceProps: Props,
                         val timeout: FiniteDuration,
                         val retryDelay: FiniteDuration,
                         val onSuccess: Persisted => Unit) extends Actor {
  import context.dispatcher

  var scheduler: Cancellable = _

  var timeoutScheduler = context.system.scheduler.scheduleOnce(
    timeout, self, ReceiveTimeout)

  val persistence = context.actorOf(persistenceProps)

  override def receive: Receive = {
    case op: TryPersist =>
      log(s"retrying $op")
      sender.path.name
      persistence ! Persist(op.key, op.valueOption, op.id)
      scheduler = context.system.scheduler.scheduleOnce(retryDelay, self, op)
    case op: Persisted =>
      log(s"successfully persisted $op")
      onSuccess(op)
      stopAll()
    case timeout: ReceiveTimeout =>
      log(s"received timeout")
      stopAll()
    case _ =>
      log _
  }

  def stopAll() = {
    persistence ! PoisonPill
    timeoutScheduler.cancel()
    scheduler.cancel()
    context.stop(self)
  }


}

