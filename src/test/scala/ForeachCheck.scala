import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import collection.mutable._

import speedy.SMap

object ForeachCheck extends Properties("SMap") {

  property("Input and foreach output list of Leaf-based map should be the same") = {
    val sizes = Gen.choose(1, 30)
    forAll (sizes) { (size: Int) =>
      val keys = (1 to size).toVector
      val map = SMap(keys.map((_, '!')): _*)
      val ks = ArrayBuffer.empty[Int]
      map.foreach(ks) { (ks, _, e) => ks += e.key }
      ks == keys
    }
  }
}
