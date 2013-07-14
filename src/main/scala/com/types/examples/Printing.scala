package com.types.examples

/**
 * User: oferr
 * Date: 7/10/13
 * Time: 8:30 PM
 */
object Printing {

  trait Print[V] extends (V => String) {
    def apply(v: V): String
  }

  object CompactPrint {

    case class LeafPrint[V]() extends Print[V] {

      def apply(v: V) = """"%s"""".format(v.toString)

    }

    case class MapPrint[V](inner: Print[V]) extends Print[Map[String, V]] {

      def apply(m: Map[String, V]) = m map {
        case (k, v) => """"%s":%s""".format(k, inner(v))
      } mkString("{", ",", "}")

    }

    case class ListPrint[V](inner: Print[V]) extends Print[List[V]] {

      def apply(els: List[V]) = els map inner mkString("[", ",", "]")

    }

  }
}

object Play extends App {

  import Printing._
  import CompactPrint._

  val m: Map[String, List[Map[String, String]]] =
    Map("a" -> List(
      Map("b" -> "c",
        "dd" -> "foo"),
      Map("e" -> "f",
        "dd" -> "bar")
    ),
      "llm" -> List(
        Map("noc" -> "l")
      )
    )

  val printer = MapPrint(ListPrint(MapPrint(LeafPrint[String]())))

  println(printer(m))
}
