import org.scalameter.api._
import speedy.SMap

object SMapBenchmark extends Bench.LocalTime {
  
  val sizes = Gen.range("size")(1, 10000, 500)

  performance of "SMap" in {
    measure method "updated" in {
      using(sizes) in { s =>
        var m = SMap.empty[Int, Int]
        for (i <- 0 until s)
          m = m.updated(i, i)
        m
      }
    }
  }

  performance of "Map" in {
    measure method "updated" in {
      using(sizes) in { s =>
        var m = Map.empty[Int, Int]
        for (i <- 0 until s)
          m = m.updated(i, i)
        m
      }
    }
  }
}
