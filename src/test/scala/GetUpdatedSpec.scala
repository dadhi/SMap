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

    // Assert.AreEqual(null, m.GetValueOrDefault(10));
    // CollectionAssert.AreEquivalent(new[] { 1 }, m.Enumerate().Select(x => x.Hash));
    // Assert.AreEqual(1, m.Count());

    // Assert.AreSame(m, m.AddOrKeep(1, "aa"));

    // var mr = m.Remove(1);
    // Assert.AreSame(ImHashMap<int, string>.Empty, mr);
    // Assert.AreEqual(0, mr.Count());

    // m = m.AddOrUpdate(2, "b");
    // Assert.AreEqual("b",  m.GetValueOrDefault(2));
    // Assert.AreEqual("a",  m.GetValueOrDefault(1));
    // Assert.AreEqual(null, m.GetValueOrDefault(10));
    // CollectionAssert.AreEquivalent(new[] { 1, 2 }, m.Enumerate().Select(x => x.Hash));
    // Assert.AreEqual(2, m.Count());

    // Assert.AreSame(m, m.AddOrKeep(1, "aa").AddOrKeep(2, "bb"));
    // Assert.AreSame(m, m.Remove(0));
    // mr = m.Remove(2);
    // Assert.AreEqual("a", mr.GetValueOrDefault(1));
    // Assert.AreEqual(1, mr.Count());

    // m = m.AddOrUpdate(3, "c");
    // Assert.AreEqual("c",  m.GetValueOrDefault(3));
    // Assert.AreEqual("b",  m.GetValueOrDefault(2));
    // Assert.AreEqual("a",  m.GetValueOrDefault(1));
    // Assert.AreEqual(null, m.GetValueOrDefault(10));
    // CollectionAssert.AreEquivalent(new[] { 1, 2, 3 }, m.Enumerate().Select(x => x.Hash));
    // Assert.AreEqual(3, m.Count());

    // Assert.AreSame(m, m.AddOrKeep(3, "aa").AddOrKeep(2, "bb").AddOrKeep(1, "cc"));
    // Assert.AreSame(m, m.Remove(0));
    // mr = m.Remove(2);
    // Assert.AreEqual("a", mr.GetValueOrDefault(1));
    // Assert.AreEqual("c", mr.GetValueOrDefault(3));
    // Assert.AreEqual(2, mr.Count());

    // m = m.AddOrUpdate(4, "d");
    // Assert.AreEqual("c",  m.GetValueOrDefault(3));
    // Assert.AreEqual("b",  m.GetValueOrDefault(2));
    // Assert.AreEqual("a",  m.GetValueOrDefault(1));
    // Assert.AreEqual("d",  m.GetValueOrDefault(4));
    // Assert.AreEqual(null, m.GetValueOrDefault(10));
    // CollectionAssert.AreEquivalent(new[] { 1, 2, 3, 4 }, m.Enumerate().Select(x => x.Hash));
    // Assert.AreEqual(4, m.Count());

    // Assert.AreSame(m, m.AddOrKeep(3, "aa").AddOrKeep(2, "bb").AddOrKeep(1, "cc"));
    // Assert.AreSame(m, m.Remove(0));

    // m = m.AddOrUpdate(5, "e");
    // Assert.AreEqual("c",  m.GetValueOrDefault(3));
    // Assert.AreEqual("b",  m.GetValueOrDefault(2));
    // Assert.AreEqual("a",  m.GetValueOrDefault(1));
    // Assert.AreEqual("d",  m.GetValueOrDefault(4));
    // Assert.AreEqual("e",  m.GetValueOrDefault(5));
    // Assert.AreEqual(null, m.GetValueOrDefault(10));
    // CollectionAssert.AreEquivalent(new[] { 1, 2, 3, 4, 5 }, m.Enumerate().Select(x => x.Hash));
    // Assert.AreEqual(5, m.Count());

    // Assert.AreSame(m, m.AddOrKeep(3, "aa").AddOrKeep(2, "bb").AddOrKeep(1, "cc"));
    // Assert.AreSame(m, m.Remove(0));

    // m = m.AddOrUpdate(6, "6");
    // Assert.AreEqual("6",  m.GetValueOrDefault(6));
    // Assert.AreEqual("e",  m.GetValueOrDefault(5));
    // Assert.AreEqual("d",  m.GetValueOrDefault(4));
    // Assert.AreEqual("c",  m.GetValueOrDefault(3));
    // Assert.AreEqual("b",  m.GetValueOrDefault(2));
    // Assert.AreEqual("a",  m.GetValueOrDefault(1));
    // Assert.AreEqual(null, m.GetValueOrDefault(10));
    // Assert.AreSame(m, m.AddOrKeep(3, "aa").AddOrKeep(2, "bb").AddOrKeep(1, "cc"));
    // CollectionAssert.AreEquivalent(new[] { 1, 2, 3, 4, 5, 6 }, m.Enumerate().Select(x => x.Hash));
    // Assert.AreEqual(6, m.Count());

    // m = m.AddOrUpdate(7, "7");
    // Assert.AreEqual("7",  m.GetValueOrDefault(7));
    // m = m.AddOrUpdate(8, "8");
    // Assert.AreEqual("8",  m.GetValueOrDefault(8));
    // m = m.AddOrUpdate(9, "9");
    // Assert.AreEqual("9",  m.GetValueOrDefault(9));
    // CollectionAssert.AreEquivalent(new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9 }, m.Enumerate().Select(x => x.Hash));
    // Assert.AreEqual(9, m.Count());

    // m = m.AddOrUpdate(10, "10");
    // Assert.AreEqual("10", m.GetValueOrDefault(10));
    // Assert.AreEqual("9",  m.GetValueOrDefault(9));
    // Assert.AreEqual("8",  m.GetValueOrDefault(8));
    // Assert.AreEqual("7",  m.GetValueOrDefault(7));
    // Assert.AreEqual("6",  m.GetValueOrDefault(6));
    // Assert.AreEqual("e",  m.GetValueOrDefault(5));
    // Assert.AreEqual("d",  m.GetValueOrDefault(4));
    // Assert.AreEqual("c",  m.GetValueOrDefault(3));
    // Assert.AreEqual("b",  m.GetValueOrDefault(2));
    // Assert.AreEqual("a",  m.GetValueOrDefault(1));
    // Assert.AreEqual(null, m.GetValueOrDefault(11));
    // Assert.AreSame(m, m.AddOrKeep(8, "8!").AddOrKeep(5, "5!").AddOrKeep(3, "aa").AddOrKeep(2, "bb").AddOrKeep(1, "cc"));
    // CollectionAssert.AreEquivalent(new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }, m.Enumerate().Select(x => x.Hash));
    // Assert.AreEqual(10, m.Count());

    // m = m.AddOrUpdate(11, "11");
    // m = m.AddOrUpdate(12, "12");
    // m = m.AddOrUpdate(13, "13");
    // Assert.AreEqual("11",  m.GetValueOrDefault(11));
    // Assert.AreEqual("12",  m.GetValueOrDefault(12));
    // Assert.AreEqual("13",  m.GetValueOrDefault(13));
    // CollectionAssert.AreEquivalent(new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 }, m.Enumerate().Select(x => x.Hash));
    // Assert.AreEqual(13, m.Count());

    // m = m.AddOrUpdate(14, "14");
    // Assert.AreEqual("14",  m.GetValueOrDefault(14));
    // Assert.AreEqual(14, m.Count());

    // m = m.AddOrUpdate(15, "15");
    // m = m.AddOrUpdate(16, "16");
    // m = m.AddOrUpdate(17, "17");
    // Assert.AreEqual("15",  m.GetValueOrDefault(15));
    // Assert.AreEqual("16",  m.GetValueOrDefault(16));
    // Assert.AreEqual("17",  m.GetValueOrDefault(17));
    // CollectionAssert.AreEquivalent(new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17 }, m.Enumerate().Select(x => x.Hash));
    // Assert.AreEqual(17, m.Count());

    // m = m.AddOrUpdate(18, "18");
    // Assert.AreEqual("18",  m.GetValueOrDefault(18));
    // CollectionAssert.AreEquivalent(new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18 }, m.Enumerate().Select(x => x.Hash));
    // Assert.AreEqual(18, m.Count());

    // var r = m.Remove(18).Remove(17).Remove(16);
    // CollectionAssert.AreEquivalent(new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }, r.Enumerate().Select(x => x.Hash));
    // Assert.IsNull(r.GetValueOrDefault(16));

    // var rr = r.Remove(16);
    // Assert.AreSame(r, rr);

    // m = m.AddOrUpdate(18, "18");
    // m = m.AddOrKeep(18, "18");
    // Assert.AreEqual("18",  m.GetValueOrDefault(18));

    // m = m.AddOrUpdate(19, "19").AddOrUpdate(20, "20").AddOrUpdate(21, "21").AddOrUpdate(22, "22").AddOrUpdate(23, "23");
    // rr = m.Remove(25).Remove(21);
    // Assert.IsNull(rr.GetValueOrDefault(21));
  }
}
