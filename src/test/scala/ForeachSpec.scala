import org.scalatest.funsuite._
import org.scalatest.matchers.should._
import scala.collection.mutable._
import speedy.SMap

class ForeachSpec extends AnyFunSuite with Matchers {
  test("Foreach of empty map ends with nothing") {
    val m = SMap.empty[Int, String]
    val l = ListBuffer.empty[Int]
    m.foreach(l) { (l, _, e) => l += e.hash }
    assert(l.isEmpty)
  }
  test("Foreach of one item map should work") {
    val m = SMap.empty + ("a" -> 1)
    val l = ListBuffer.empty[String]
    m.foreach(l) { (l, _, e) => l += e.key }
    l shouldBe ListBuffer("a")
  }
  test("Foreach of 2 items map should work") {
    val m = SMap.empty + ("a" -> 1) + ("b" -> 2)
    val l = ListBuffer.empty[String]
    m.foreach(l) { (l, _, e) => l += e.key }
    l shouldBe ListBuffer("a", "b")
  }
}
