import speedy.SMap
import org.scalatest.funsuite._
import org.scalameter._

class Benchmark extends AnyFunSuite {

  val bm = config(
    Key.exec.benchRuns := 20,
    Key.verbose := true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
    // } withMeasurer {
    //   new Measurer.MemoryFootprint
  }

  val mapSize = 100

  test("Add items to Map") {
    val result = bm measure {
      var m = Map.empty[Int, String]
      for (i <- 0 until mapSize)
        m = m + (i -> "m")
      m
    }
    println(s"Adding 10 items to Map: $result")
  }

  test("Add items to SMap") {
    val result = bm measure {
      var m = SMap.empty[Int, String]
      for (i <- 0 until mapSize)
        m = m + (i -> "s")
      m
    }
    println(s"Adding 100 items to SMap: $result")
  }
}
