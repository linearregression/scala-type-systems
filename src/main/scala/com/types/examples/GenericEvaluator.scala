package com.types.examples

import GenericEvaluator._
import Evaluatables._

import java.util.{List => JList}

/**
 * User: oferr
 * Date: 7/11/13
 * Time: 5:32 PM
 */
abstract class GenericEvaluator[A,B] private (path:Seq[Evaluate]) {

  def evaluate(a:A):Option[B]

  def atKey[C,M[_,_]](key:String)(implicit ev:B <:< M[String,C] , mapView:EvaluatableByKey[M]):GenericEvaluator[A,C] = new GenericEvaluator[A,C](path :+ AtKey(key)) {
    def evaluate(a: A) = GenericEvaluator.this.evaluate(a) flatMap (b => mapView.get(ev(b),key))
  }

  def atIndex[C,L[_]](index:Int)(implicit ev:B <:< L[C],idxView:EvaluatableAtIndex[L]):GenericEvaluator[A,C] = new GenericEvaluator[A,C](path :+ AtIndex(index)) {
    def evaluate(a: A) = GenericEvaluator.this.evaluate(a) flatMap (b => idxView.get(ev(b),index))
  }

}

object GenericEvaluator {

  def forType[A] = new GenericEvaluator[A,A](Nil) {
    def evaluate(a: A) = Some(a)
  }

  def apply[A](a:A) = forType[A]

  sealed trait Evaluate

  case class AtKey(key:String) extends Evaluate

  case class AtIndex(index:Int) extends Evaluate
}

object GenEval extends App {

  import Evaluator._
  import Evaluatables._

  val m:Map[String,List[Map[String,List[String]]]] = Map.empty

  val ev = GenericEvaluator.forType[m.type] atKey "foo" atIndex 0 atKey "bar"

  val n:Map[String,JList[Map[String,List[String]]]] = Map.empty

  val nev = GenericEvaluator.forType[n.type] atKey "foo" atIndex 0 atKey "bar"
}

