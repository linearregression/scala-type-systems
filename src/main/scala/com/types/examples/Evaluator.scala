package com.types.examples

import Evaluator._

/**
 * User: oferr
 * Date: 7/11/13
 * Time: 4:07 PM
 */

abstract class Evaluator[A,B] private (path:Seq[Evaluate]) {

  def evaluate(a:A):Option[B]

  def atKey[C](key:String)(implicit ev:B <:< Map[String,C]):Evaluator[A,C] = new Evaluator[A,C](path :+ AtKey(key)) {
    def evaluate(a: A) = Evaluator.this.evaluate(a) flatMap (b => ev(b).get(key))
  }

  def atIndex[C](index:Int)(implicit ev:B <:< List[C]):Evaluator[A,C] = new Evaluator[A,C](path :+ AtIndex(index)) {
    def evaluate(a: A) = Evaluator.this.evaluate(a) flatMap (b => ev(b).lift(index))
  }

}

object Evaluator {

  def forType[A] = new Evaluator[A,A](Nil) {
    def evaluate(a: A) = Some(a)
  }

  def apply[A](a:A) = forType[A]

  sealed trait Evaluate

  case class AtKey(key:String) extends Evaluate

  case class AtIndex(index:Int) extends Evaluate
}

object Eval extends App {

  import Evaluator._

  val m:Map[String,List[Map[String,List[String]]]] = Map.empty

  val ev = Evaluator.forType[m.type] atKey "foo" atIndex 0 atKey "bar"
}




