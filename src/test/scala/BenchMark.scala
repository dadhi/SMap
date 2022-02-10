import org.scalatest.funsuite._
import org.scalameter._

class BenchMark extends AnyFunSuite {
  test("Range") {
    val time = config(
      Key.exec.benchRuns := 10,
      Key.verbose := true
    ) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    // } withMeasurer {
    //   new Measurer.MemoryFootprint
    } measure {
      for (i <- 0 until 10000) yield i
    }
    println(s"Time: $time")
  }
}
