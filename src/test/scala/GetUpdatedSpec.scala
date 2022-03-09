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

  test("Updating the rather big map and checking it on every update") {
    var m = SMap.empty[Int, String]

    m.getOrElse(0, "") shouldBe ""
    m.getOrElse(13, null) shouldBe null
    m.size shouldBe 0
    m.toList.isEmpty shouldBe true

    m = m.updated(1, "a");
    m.getOrElse(1, "") shouldBe "a"
    m.getOrElse(10, null) shouldBe null
    m.size shouldBe 1
    m.toList shouldBe List(1 -> "a")

    m.updatedOrKept(1, "aa") shouldBe m
    m.updatedOrKept(2, "b").toList shouldBe List(1 -> "a", 2 -> "b")

    var mr = m.removed(1)
    mr.isEmpty shouldBe true

    m = m.updated(2, "b");
    m.getOrElse(2, null) shouldBe "b"
    m.getOrElse(1, null) shouldBe "a"
    m.getOrElse(10, null) shouldBe null
    m.size shouldBe 2
    m.toList shouldBe List(1 -> "a", 2 -> "b")

    m.updatedOrKept(1, "aa").updatedOrKept(2, "bb") shouldBe m
    m.removed(0) shouldBe m
    mr = m.removed(2)
    mr.getOrElse(1, null) shouldBe "a"
    mr.size shouldBe 1

    m = m.updated(3, "c");
    m.toList shouldBe List(1 -> "a", 2 -> "b", 3 -> "c")

    m.updatedOrKept(3, "aa")
      .updatedOrKept(2, "bb")
      .updatedOrKept(1, "cc") shouldBe m
    m.removed(0) shouldBe m
    mr = m.removed(2)
    mr.getOrElse(1, null) shouldBe "a"
    mr.getOrElse(3, null) shouldBe "c"
    mr.size shouldBe 2

    m = m.updated(4, "d")
    m.size shouldBe 4
    m.toList shouldBe List(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d")

    m.updatedOrKept(3, "aa")
      .updatedOrKept(2, "bb")
      .updatedOrKept(1, "cc") shouldBe m
    m.removed(0) shouldBe m

    m = m.updated(5, "e")
    m.size shouldBe 5
    m.toList shouldBe List(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d", 5 -> "e")

    m.updatedOrKept(3, "aa")
      .updatedOrKept(2, "bb")
      .updatedOrKept(1, "cc") shouldBe m
    m.removed(0) shouldBe m

    m = m.updated(6, "6")
    m.size shouldBe 6
    m.toList shouldBe List(
      1 -> "a",
      2 -> "b",
      3 -> "c",
      4 -> "d",
      5 -> "e",
      6 -> "6"
    )

    m = m.updated(7, "7").updated(8, "8").updated(9, "9")
    m.size shouldBe 9
    m.toList shouldBe List(
      1 -> "a",
      2 -> "b",
      3 -> "c",
      4 -> "d",
      5 -> "e",
      6 -> "6",
      7 -> "7",
      8 -> "8",
      9 -> "9"
    )

    m = m.updated(10, "10")
    assert(Some("10") == m.get(10))
    assert(Some("9") == m.get(9))
    assert(Some("8") == m.get(8))
    assert(Some("7") == m.get(7))
    assert(Some("6") == m.get(6))
    assert(Some("e") == m.get(5))
    assert(Some("d") == m.get(4))
    assert(Some("c") == m.get(3))
    assert(Some("b") == m.get(2))
    assert(Some("a") == m.get(1))
    assert(None == m.get(11))
    assert(
      m == m
        .updatedOrKept(8, "8!")
        .updatedOrKept(5, "5!")
        .updatedOrKept(3, "aa")
        .updatedOrKept(2, "bb")
        .updatedOrKept(1, "cc")
    )
    m.size shouldBe 10
    m.toList shouldBe List(
      1 -> "a",
      2 -> "b",
      3 -> "c",
      4 -> "d",
      5 -> "e",
      6 -> "6",
      7 -> "7",
      8 -> "8",
      9 -> "9",
      10 -> "10"
    )

    m = m.updated(11, "11").updated(12, "12").updated(13, "13")
    m.size shouldBe 13
    m.toList shouldBe List(
      1 -> "a",
      2 -> "b",
      3 -> "c",
      4 -> "d",
      5 -> "e",
      6 -> "6",
      7 -> "7",
      8 -> "8",
      9 -> "9",
      10 -> "10",
      11 -> "11",
      12 -> "12",
      13 -> "13"
    )

    m = m.updated(14, "14");
    assert("14" == m.getOrElse(14, null))
    assert(14 == m.size)

    m = m.updated(15, "15").updated(16, "16").updated(17, "17")
    assert(Some("15") == m.get(15))
    assert(Some("16") == m.get(16))
    assert(Some("17") == m.get(17))
    assert(17 == m.size)
    m.toList shouldBe List(
      1 -> "a",
      2 -> "b",
      3 -> "c",
      4 -> "d",
      5 -> "e",
      6 -> "6",
      7 -> "7",
      8 -> "8",
      9 -> "9",
      10 -> "10",
      11 -> "11",
      12 -> "12",
      13 -> "13",
      14 -> "14",
      15 -> "15",
      16 -> "16",
      17 -> "17"
    )

    m = m.updated(18, "18")
    assert("18" == m.getOrElse(18, null))
    assert(18 == m.size)
    m.toList shouldBe List(
      1 -> "a",
      2 -> "b",
      3 -> "c",
      4 -> "d",
      5 -> "e",
      6 -> "6",
      7 -> "7",
      8 -> "8",
      9 -> "9",
      10 -> "10",
      11 -> "11",
      12 -> "12",
      13 -> "13",
      14 -> "14",
      15 -> "15",
      16 -> "16",
      17 -> "17",
      18 -> "18"
    )

    var r = m.removed(18).removed(17).removed(16)
    assert(None == r.get(16))
    r.toList shouldBe List(
      1 -> "a",
      2 -> "b",
      3 -> "c",
      4 -> "d",
      5 -> "e",
      6 -> "6",
      7 -> "7",
      8 -> "8",
      9 -> "9",
      10 -> "10",
      11 -> "11",
      12 -> "12",
      13 -> "13",
      14 -> "14",
      15 -> "15"
    )

    m = m.updated(18, "18").updatedOrKept(18, "18")
    assert("18" == m.getOrElse(18, null))

    m = m
      .updated(19, "19")
      .updated(20, "20")
      .updated(21, "21")
      .updated(22, "22")
      .updated(23, "23")
    r = m.removed(25).removed(21)
    assert(r.get(21).isEmpty)
  }

  class Xk[K](val key: K) {
    override def hashCode(): Int = 1
    override def equals(o: Any): Boolean = o match {
      case x: Xk[K] => key == x.key
      case _        => false
    }
  }

  object Xk {
    def apply[K](key: K) = new Xk[K](key)
  }

  test("adding the conflicting keys should be fun") {
    var m = SMap.empty[Xk[Int], String]

    assert(None == m.get(Xk(0)))
    assert(None == m.get(Xk(13)))

    m = m + ((Xk(1), "a")) + ((Xk(2), "b"))

    assert(Some("a") == m.get(Xk(1)))
    assert(Some("b") == m.get(Xk(2)))
    assert(None      == m.get(Xk(10)))

    var mr = m.removed(Xk(1))
    assert(None == mr.get(Xk(1)))
    assert(Some("b") == mr.get(Xk(2)))
    
    m = m.updated(Xk(3), "c")
    mr = m.removed(Xk(2))
    assert(mr.size == 2)
    assert(None == mr.get(Xk(2)))
    assert(Some("a") == mr.get(Xk(1)))
    assert(Some("c") == mr.get(Xk(3)))
  }
}
