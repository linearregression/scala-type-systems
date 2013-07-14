package com.types.examples

import java.util.{List => JList}
import scala.collection.JavaConverters._
/**
 * User: oferr
 * Date: 7/11/13
 * Time: 5:22 PM
 */
object Evaluatables {

  trait EvaluatableAtIndex[L[_]]  {

    def apply[E](els:L[E],i:Int):E

    def get[E](els:L[E],i:Int):Option[E]

    def values[E](els:L[E]):Iterable[E]

  }

  trait EvaluatableByKey[M[_,_]] {

    def apply[K,V](map:M[K,V],key:K):V

    def get[K,V](map:M[K,V],key:K):Option[V]

    def values[K,V](map:M[K,V]):Iterable[(K,V)]

  }

  implicit object ListIsEvaluatableAtIndex extends EvaluatableAtIndex[List] {
    def apply[E](els: List[E], i: Int) = els(i)

    def get[E](els: List[E], i: Int) = els.lift(i)

    def values[E](els: List[E]) = els
  }

  implicit object JListEvaluatableAtIndex extends EvaluatableAtIndex[JList] {
    def apply[E](els: JList[E], i: Int) = els.get(i)

    def get[E](els: JList[E], i: Int) = if (i >= 0 && i < els.size()) Some(els.get(i)) else None

    def values[E](els: JList[E]) = els.asScala
  }

  implicit object MapIsEvaluatableByKey extends EvaluatableByKey[Map] {
    def apply[K, V](map: Map[K, V], key: K) = map(key)

    def get[K, V](map: Map[K, V], key: K) = map.get(key)

    def values[K, V](map: Map[K, V]) = map.toSeq
  }



}
