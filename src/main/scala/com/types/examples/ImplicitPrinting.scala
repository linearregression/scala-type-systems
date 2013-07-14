package com.types.examples


/**
 * User: oferr
 * Date: 7/11/13
 * Time: 3:38 PM
 */

trait Print[V] extends (V => String) {
  def apply(v: V): String
}

object Print {

  def apply[V](v:V)(implicit p:Print[V]) = p(v)

  implicit def IdentityPrint[V]:Print[V] = new Print[V] {
    def apply(v: V) = v.toString
  }

  object Printers {

    implicit def printMap[V](implicit p:Print[V]):Print[Map[String,V]] = new Print[Map[String,V]] {
      def apply(m: Map[String, V]) = m map {
        case (k, v) => """"%s":%s""".format(k, p(v))
      } mkString("{", ",", "}")

    }

    implicit def printList[V](implicit p:Print[V]):Print[List[V]] = new Print[List[V]] {

      def apply(els: List[V]) = els map p mkString("[", ",", "]")

    }
  }

}

object ImplicitPlay extends App {

  import Print.apply
  import Print.Printers._

  val m: Map[String, List[Map[String, String]]] = Map()


  println(Print(m))

}