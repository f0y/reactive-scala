package kvstore

import akka.actor._
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{AskTimeoutException, ask, pipe}
import scala.concurrent.duration._
import akka.util.Timeout
import scala.util.{Random, Failure, Success}
import scala.concurrent.Future
import kvstore.PersistenceManager.TryPersist
import scala.Some
import scala.collection.mutable.{Set => MSet}
import kvstore.Arbiter.Replicas
import kvstore.PersistenceManager.TryPersist


object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case class CheckReplication(id: Long)

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  arbiter ! Join

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var waitingAck = Map.empty[Long, (ActorRef, MSet[ActorRef])]

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica(0))
  }

  def replicate(key: String, id: Long, value: Option[String]) {
    val manager = context.actorOf(PersistenceManager.props(
      persistenceProps, 1.second, 100.milliseconds,
      _ => self ! Replicated(key, id)))
    manager ! TryPersist(key, value, id)

    waitingAck += id ->(sender, MSet.empty[ActorRef])
    val (_, awaitingReplicas) = waitingAck.get(id).get
    replicators.foreach(a => {
      a ! Replicate(key, value, id)
      awaitingReplicas.add(a)
    })
    awaitingReplicas.add(self)
    context.system.scheduler.scheduleOnce(1.second, self, CheckReplication(id))
  }
  
  val leader: Receive = {
    case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
    case Insert(key, value, id) =>
      kv += key -> value
      replicate(key, id, Some(value))
    case Remove(key, id) =>
      kv -= key
      replicate(key, id, None)
    case Replicas(replicas) =>
      val newReplicas = replicas - self -- secondaries.keys
      val removedReplicas = secondaries.keys.toSet -- replicas
      removedReplicas.foreach(re => {
        secondaries.get(re) match {
          case Some(replicator) =>
            replicator ! PoisonPill
            secondaries = secondaries - re
            re ! PoisonPill
            for {
              (_, awaitingReplicas) <- waitingAck.values
            } yield awaitingReplicas.remove(replicator)
          case None => log("OOOOps")
        }
      })

      newReplicas.foreach(re => {
        val replicator = context.actorOf(Replicator.props(re))
        replicators = replicators + replicator
        secondaries += re -> replicator
        for {
          (k, v) <- kv
          i <- Range(0, kv.size)
        } yield replicator ! Replicate(k, Some(v), i)
      })

    case Replicated(key, id) =>
      log(s"replication done for key=$key, id=$id")
      waitingAck.get(id) match {
        case Some((initiator, awaitingReplicas)) =>
          log(s"initiator=${initiator.path.name} awaiting=${awaitingReplicas.map(_.path.name)}")
          awaitingReplicas.remove(sender)
          if (awaitingReplicas.isEmpty) {
            initiator ! OperationAck(id)
            waitingAck = waitingAck - id
          }
        case None => log(s"newly connected replica gets replicated")
      }

    case CheckReplication(id) =>
      log(s"checking replication status for id=$id")
      waitingAck.get(id) match {
        case Some((initiator, awaitingReplicas)) =>
          if (!awaitingReplicas.isEmpty) {
            initiator ! OperationFailed(id)
            waitingAck = waitingAck - id
          }
        case None => log ("OOOOOps")
      }

  }


  def replica(nextSeq: Long): Receive = {
    case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
    case Snapshot(key, value, seq) =>
      if (nextSeq == seq) {
        val origin = sender
        val manager = context.actorOf(PersistenceManager.props(
          persistenceProps, 1.second, 100.milliseconds,
          persisted => origin ! SnapshotAck(persisted.key, persisted.id)))
        manager ! TryPersist(key, value, seq)
        value match {
          case None => kv -= key
          case Some(rawVal) =>
            kv += key -> rawVal
        }
        context.become(replica(seq + 1))
      } else if (nextSeq > seq) {
        sender ! SnapshotAck(key, seq)
      }


  }

}

