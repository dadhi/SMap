import scala.collection.mutable._
import speedy.SMap
import org.scalatest.funsuite._

class SMapForeachSpec extends AnyFunSuite {
  test("Empty map is empty map") {
    val m = SMap.empty[Int, String]
    val l = ListBuffer.empty[Int]
    m.foreach(()) { (_, _, e) => l += e.hash }
    assert(l.isEmpty)
  }
}
