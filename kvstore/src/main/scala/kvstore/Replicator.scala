package kvstore

import akka.actor.{ReceiveTimeout, Props, Actor, ActorRef}
import akka.pattern.{AskTimeoutException, ask}
import scala.concurrent.duration._
import akka.util.Timeout

import scala.actors.threadpool.TimeoutException
import scala.util.{Failure, Success}
import kvstore.Persistence.{Persist, Persisted}
import scala.concurrent.Future

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)
  case class RetrySnapshot(snapshot: Snapshot, timeout: Boolean)


  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  def receive: Receive = {
    case repl @ Replicate(key, value, id) =>
      log(s"replicate request $repl")
      val seq = nextSeq
      val snapshot = Snapshot(key, value, seq)
      context.system.scheduler.scheduleOnce(
        200.milliseconds, self, RetrySnapshot(snapshot, timeout = false))
      context.system.scheduler.scheduleOnce(
        1.second, self, RetrySnapshot(snapshot, timeout = true))
      acks += seq -> (sender, Replicate(key, value, id))
      replica ! snapshot

    case RetrySnapshot(snapshot @ Snapshot(key, value, seq), isTimeout) =>
      log(s"retrying snapshot $snapshot")
      if (acks.get(seq) != None && !isTimeout) {
        context.system.scheduler.scheduleOnce(
          200.milliseconds, self, RetrySnapshot(snapshot, timeout = false))
        replica ! snapshot
      } else if (isTimeout) {
        acks = acks - seq
      }

    case ack @ SnapshotAck(key, seq) =>
      log(s"received snapshot ack $ack")
      acks.get(seq) match {
        case Some((origin, req)) =>
          origin ! Replicated(key, req.id)
        case None => log(s"Already timeout for $seq")
      }
      acks = acks - seq

  }

}
