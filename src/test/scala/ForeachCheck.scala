import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import collection.mutable._

import speedy.SMap

object ForeachCheck extends Properties("SMap.Iterable") {

  property("foreachWith") = {
    val sizes = Gen.choose(1, 7)
    forAll (sizes) { (size: Int) =>
      val keys = (1 to size).toVector
      val map = SMap(keys.map((_, '!')): _*)
      val ks = ArrayBuffer.empty[Int]
      map.foreachWith(ks) { (ks, _, e) => ks += e.key }
      ks == keys
    }
  }

  property("foreachWith") = {
    val sizes = Gen.choose(8, 100)
    forAll (sizes) { (size: Int) =>
      val keys = (1 to size).toVector
      val map = SMap(keys.map((_, '!')): _*)
      val ks = ArrayBuffer.empty[Int]
      map.foreachWith(ks) { (ks, _, e) => ks += e.key }
      ks == keys
    }
  }
}
