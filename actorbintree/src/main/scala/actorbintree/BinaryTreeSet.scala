/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import akka.event.LoggingReceive
import java.util.concurrent.atomic.AtomicLong
import actorbintree.BinaryTreeNode.CopyFinished

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef

    def id: Int

    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection */
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {

  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = LoggingReceive {
    case op: Operation => root ! op
    case GC =>
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)

  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case op: Operation => pendingQueue = pendingQueue.enqueue(op)
    case CopyFinished =>
      pendingQueue.foreach(newRoot ! _)
      pendingQueue = Queue.empty[Operation]
      root = newRoot
      context.become(normal)
  }

}

object BinaryTreeNode {

  trait Position

  case object Left extends Position

  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)

  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal


  def determinePos(existing: Int, newElem: Int): Option[Position] = {
    if (newElem > existing) {
      return Some(Right)
    } else if (newElem < existing) {
      return Some(Left)
    }
    None
  }

  def actOnTree(position: Option[Position], leafAction: => Position => Unit, nextAction: => ActorRef => Unit, selfAction: => Unit) {
    position match {
      case Some(pos) =>
        subtrees.get(pos) match {
          case Some(next) =>
            nextAction(next)
          case _ =>
            leafAction(pos)
        }
      case None => selfAction
    }
  }

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = LoggingReceive {
    case Insert(requester, id, newElem) =>
      actOnTree(
      determinePos(elem, newElem),
      pos => {
        subtrees += pos ->
          context.actorOf(BinaryTreeNode.props(newElem, initiallyRemoved = false))
        requester ! OperationFinished(id)
      },
      next => {
        next ! Insert(requester, id, newElem)
      }, {
        removed = false
        requester ! OperationFinished(id)
      }
      )
    case Contains(requester, id, newElem) =>
      actOnTree(
      determinePos(elem, newElem),
      pos => {
        requester ! ContainsResult(id, result = false)
      },
      next => {
        next ! Contains(requester, id, newElem)
      }, {
        if (!removed) {
          requester ! ContainsResult(id, result = true)
        } else {
          requester ! ContainsResult(id, result = false)
        }
      }
      )
    case Remove(requester, id, newElem) =>
      actOnTree(
      determinePos(elem, newElem),
      pos => {
        requester ! OperationFinished(id)
      },
      next => {
        next ! Remove(requester, id, newElem)
      }, {
        removed = true
        requester ! OperationFinished(id)
      }
      )
    case CopyTo(treeNode) => {
      if (!removed) {
        treeNode ! Insert(self, 0, elem)
      }
      subtrees.values.foreach(_ ! CopyTo(treeNode))
      if (subtrees.values.isEmpty && removed) {
        context.parent ! CopyFinished
        self ! PoisonPill
      } else {
        context.become(copying(subtrees.values.toSet, removed))
      }
    }
  }

  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {

    case OperationFinished(id) =>
      if (expected.isEmpty) {
        context.parent ! CopyFinished
        self ! PoisonPill
      } else {
        context.become(copying(expected, true))
      }
    case CopyFinished =>
      val waiting = expected - sender
      if (waiting.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        self ! PoisonPill
      } else {
        context.become(copying(waiting, insertConfirmed))
      }
  }

}
