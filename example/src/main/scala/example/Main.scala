package example


/**
 * Created with IntelliJ IDEA.
 * User: kandaurov
 * Date: 2/14/14
 * Time: 2:52 PM
 */
object Main {

  val integers = new Generator[Int] {
    val rand = new java.util.Random
    override def generate: Int = rand.nextInt()
  }

  val booleans = for (x <- integers) yield x > 0

  def randTree: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leaf else inner
  } yield tree

  def leaf = for {
    x <- integers
  } yield Leaf(x)

  def inner = for {
    x <- randTree
    y <- randTree
  } yield Inner(x, y)



  def main(args: Array[String]) {
    println(randTree.generate)

  }

}

trait Tree

case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}


