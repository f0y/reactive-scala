package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.concurrent.duration._
import java.util.NoSuchElementException
import scala.collection.immutable.Range.Inclusive
import java.util.concurrent.TimeoutException
import scala.concurrent.TimeoutException

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Future blocks for duration") {
    val DELAY = 1.second
    val delayFuture = Future.delay(DELAY)
    val start = System.nanoTime()
    Await.result(delayFuture, 2 seconds)
    val end = System.nanoTime()
    assert(((end - start) milliseconds) minus DELAY gt (0 seconds))
  }

  test("Non blocking value retrieving") {
    var f = Future.never[Int]
    try {
      f.now
    } catch {
      case t: NoSuchElementException =>
    }
    f = Future.always(5)
    assert(f.now == 5)
  }

  test("'All' method") {
    val futures = List.fill(4)(Future.always(1))
    val all = Future.all(futures)
    assert(Await.result(all, 0 seconds) == List.fill(4)(1))
  }

  test("'Any' method") {
    val futures = List(Future{Thread.sleep(500); 1}, Future.always(2))
    val result: Int = Await.result(Future.any(futures), 0 seconds)
    assert(result == 2)
  }

  test("'ContinueWith' happy path") {
    val future = Future.always(1) continueWith { x => x.now + 1 }
    val result = Await.result(future, 0 nanos)
    assert(result == 2)
  }

  test("'ContinueWith' laziness") {
    intercept[TimeoutException] {
      val f = Future.delay(2 seconds) continueWith {_ => assert(false) }
      Await.result(f, 0.5 seconds)
    }
  }

  test("'Continue' happy path") {
    val future = Future.always(1) continue {
      case Success(x) => x + 1
      case Failure(exc) => throw exc
    }
    val result = Await.result(future, 0 nanos)
    assert(result == 2)
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("Allow to cancel future task") {
    @volatile var isRunning = false
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          isRunning = true
        }
        isRunning = false
      }
    }
    Future.delay(0.1 seconds) onSuccess {
      case _ =>
        assert(isRunning)
        working.unsubscribe()
        assert(!isRunning)
    }
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




