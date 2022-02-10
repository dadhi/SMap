package example

object ParseDoubleApp {
  def main(args: Array[String]): Unit = {
    val s = args(0)
    val d = parseDoubleFromString(s)
    println(s"Parsed double: $d")
  }

  def parseDoubleFromString(s: String): Option[Double] = {
    println("input:  " + s)

    var dotIndex = s.indexOf('.')
    if (dotIndex == -1) {
      val d: Double = s.toInt
      Some(d)
    }
    else {
      val wholePart = s.substring(0, dotIndex)
      val floatPart = s.substring(dotIndex + 1)

      // 46 -> 0.46 = 46 / 10 pow 2
      val f = floatPart.toInt / Math.pow(10, floatPart.length)
      val w = wholePart.toInt
      val d: Double = w + f
      println("output: " + d)
      Some(d)
    }
  }
}
