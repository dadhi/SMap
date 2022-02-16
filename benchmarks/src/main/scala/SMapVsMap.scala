package benchmarks

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import speedy.SMap

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@Warmup(iterations = 5, timeUnit = TimeUnit.SECONDS, time = 1)
@Measurement(iterations = 5, timeUnit = TimeUnit.SECONDS, time = 1)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
class SMapVsMap {

  @Param(Array("1", "10", "100", "1000"))
  var size: Int = _

  @Benchmark
  def measureSMapUpdated: SMap[Int, Int] = {
    var m = SMap.empty[Int, Int]
    for (i <- 0 until size)
      m = m + ((i -> i))
    m
  }

  @Benchmark
  def measureMapUpdated: Map[Int, Int] = {
    var m = Map.empty[Int, Int]
    for (i <- 0 until size)
      m = m + ((i -> i))
    m
  }

  /**
   * v1.0.0-preview-01 results
   * 
    Benchmark                     (size)  Mode  Cnt       Score       Error  Units
    SMapVsMap.measureMapUpdated        1  avgt    5       4.690 ∩┐╜     0.176  ns/op
    SMapVsMap.measureMapUpdated       10  avgt    5     938.134 ∩┐╜   129.203  ns/op
    SMapVsMap.measureMapUpdated      100  avgt    5   11587.144 ∩┐╜   802.079  ns/op
    SMapVsMap.measureMapUpdated     1000  avgt    5  190914.380 ∩┐╜  8471.812  ns/op
    SMapVsMap.measureSMapUpdated       1  avgt    5       6.342 ∩┐╜     0.101  ns/op
    SMapVsMap.measureSMapUpdated      10  avgt    5     200.197 ∩┐╜    38.809  ns/op
    SMapVsMap.measureSMapUpdated     100  avgt    5    6046.650 ∩┐╜  1570.872  ns/op
    SMapVsMap.measureSMapUpdated    1000  avgt    5  126266.188 ∩┐╜ 19281.859  ns/op
   */
}
