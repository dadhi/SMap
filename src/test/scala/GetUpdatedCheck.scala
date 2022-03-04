import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import collection.mutable._

import speedy.SMap

object GetUpdatedCheck extends Properties("SMap") {

  property("updated") = {
    val lists = Gen.containerOfN[List, Int](5, Gen.choose(1, 100000))
    forAll (lists) { list =>
      var map = SMap.empty[Int, Int]
      for (n <- list)
        map = map.updated(n, n)

      val values = new ArrayBuffer[Int](list.size)
      for (n <- list)
        values += map.getOrElse(n, 0)

      "Input list: #1" |: list == values.toList
    }
  }
}
