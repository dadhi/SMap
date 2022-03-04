import org.scalatest.funsuite._
import org.scalatest.matchers.should._
import scala.collection.mutable._
import speedy.SMap

class GetUpdatedSpec extends AnyFunSuite with Matchers {
  test("Ensure we get all updated items back in order") {
    val lists = List(
      List("0", "-1", "3", "1", "-2").map(_.toInt),
      List("2", "-2", "0", "1", "-1").map(_.toInt)
    )

    for (list <- lists) {
      var map = SMap.empty[Int, Int]
      for (n <- list)
        map = map.updated(n, n)

      val values = new ArrayBuffer[Int](list.size)
      for (n <- list)
        values += map.getOrElse(n, 0)

      list should equal(values.toList)
    }
  }
}
