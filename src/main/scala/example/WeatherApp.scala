package example

import core.Weather
import scala.concurrent._, duration._

object WeatherApp {
  def main(args: Array[String]): Unit = {
    val w = Await.result(Weather.weather, 10.seconds)
    println(s"Hello! The weather in New York is $w.")
    Weather.http.close()
  }
}