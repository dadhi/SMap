import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

case class Foo[@specialized(Int) T](bar: T)
Foo(42).getClass.getName
Foo("").getClass.getName

trait M[@specialized(Int) K, V]

object M {

  abstract class E[K, V](i: Int) extends M[K, V]

  final case class KVE[K, V](i: Int, k: K) extends E[K, V](i)

  final case class VE[V](i: Int) extends E[Int, V](i)
}

val s = M.KVE(1, 1)
s.getClass.getName

List("0", "-1", "3", "1", "-2").map(_.toInt)

Map(1 -> 2).foreach { case (k, v) => println(s"$k -> $v") }

Some("x").getOrElse("")

val x: Option[String] = Some("x")

object Extractor {
  def extract[T](list: List[Any])(implicit tag: ClassTag[T]) = list.flatMap {
    case element: T => Some(element)
    case _          => None
  }
}

Extractor.extract[Int](List(1, "string1", List(), "string2"))
Extractor.extract[List[Int]](List(List(1, 2), List("a", "b")))

object Recognizer {
  def recognize[T](x: T)(implicit tag: TypeTag[T]): String =
    tag.tpe match {
      case TypeRef(utype, usymbol, args) =>
        List[Object](utype, usymbol, args).mkString(", ")
    }
}
Recognizer.recognize(List(1, 2))

val (a, b, c) = (1, 2, 3)

import scala.collection.mutable.ArrayBuffer
val buffer = ArrayBuffer[Int]()
buffer += 1
