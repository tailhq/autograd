package com.kogecoo.scalaad.graph

import shapeless.Nat

import java.util.concurrent.ConcurrentHashMap

import scala.Predef.{any2stringadd => _, _}
import scala.language.higherKinds
import scala.collection.JavaConverters._


class Grad[G <: Nat](val grad: ConcurrentHashMap[ValueExpr[_ <: Nat], ValueExpr[G]]) {

  def size: Int = grad.size

  def apply(x: ValueExpr[_ <: Nat]): Option[ValueExpr[_ <: Nat]] = Option(grad.get(x))

}

object Grad {

  def apply[G <: Nat](p: ValueExpr[_ <: Nat], v: ValueExpr[G]): Grad[G] = {
    val m = new ConcurrentHashMap(Map(p -> v).asJava)
    new Grad[G](m)
  }

  def empty[G <: Nat]: Grad[G] = {
    val m = new ConcurrentHashMap[ValueExpr[_ <: Nat], ValueExpr[G]](Map.empty.asJava)
    new Grad(m)
  }

  implicit class GradOp[G <: Nat](val self: Grad[G]) extends AnyVal {
    def ++(other: Grad[G]): Grad[G] = {
      val keys = self.grad.keySet | other.grad.keySet
      val m = (for {
        k <-  keys
        v1 =  self.grad.get(k)
        v2 =  other.grad.get(k)
        if v1.isDefined || v2.isDefined
      } yield {
        if (v1.isDefined && v2.isDefined) {
          (k, v1.get + v2.get)
        } else if (v1.isDefined) {
          (k, v1.get)
        } else {
          (k, v2.get)
        }
      }).toMap[ValueExpr[_ <: Nat], ValueExpr[G]]
      new Grad(m)
    }

  }

}

  /*def where(cond: BooleanExpr[_ <: S], g1: Grad, g2: Grad): Grad = {
    val keys = g1.grad.keySet | g2.grad.keySet
    val (m: Map[ValueExpr[_ <: S], ValueExpr[_ <: S]]) = (for (k <- keys) yield {

      val value: Option[ValueExpr[_ <: S]] = (g1.grad.get(k), g2.grad.get(k)) match {
        case (Some(a), Some(b)) => Some(makeWhere(cond, a, b))
        case (Some(a), None)    => Some(makeWhere(cond, a, makeZero(a.shape)))
        case (None,    Some(b)) => Some(makeWhere(cond, makeZero(b.shape), b))
        case (None,    None)    => None
      }
      value.map((k, _))
    }).flatten.toMap
    new Grad(m)
  }

  private[this] def makeZero(zeroShape: S): ValueExpr[_ <: S] = zeroShape match {
    case s: S0 => Zero0()
    case s: S1 => Zero1(s)
    case s: S2 => Zero2(s)
  }

  private[this] def makeWhere(cond: BooleanExpr[_ <: S], n1: ValueExpr[_ <: S], n2: ValueExpr[_ <: S]): ValueExpr[_ <: S] = {
    (cond, cond.shape, n1, n1.shape, n2, n2.shape) match {
      // instead of a.asInstanceOf[N0]
      case (a: B0 @unchecked, _: S0, b: V0 @unchecked, _: S0, c: V0 @unchecked, _: S0) => Where0_0(a, b, c)
      case (a: B0 @unchecked, _: S0, b: V1 @unchecked, _: S1, c: V1 @unchecked, _: S1) => Where0_1(a, b, c)
      case (a: B0 @unchecked, _: S0, b: V2 @unchecked, _: S2, c: V2 @unchecked, _: S2) => Where0_2(a, b, c)
      //case (a: B1 @unchecked, _: S1, b: N0 @unchecked, _: S0, c: N0 @unchecked, _: S0) => Where1_0(a, b, c)
      case (a: B1 @unchecked, _: S1, b: V1 @unchecked, _: S1, c: V1 @unchecked, _: S1) => Where1_1(a, b, c)
//case (a: B1 @unchecked, _: S1, b: V2 @unchecked, _: S2, c: V2 @unchecked, _: S2) => Where1_2(a, b, c)
      //case (a: B2 @unchecked, _: S2, b: N0 @unchecked, _: S0, c: N0 @unchecked, _: S0) => Where2_0(a, b, c)
      //case (a: B2 @unchecked, _: S2, b: N1 @unchecked, _: S1, c: N1 @unchecked, _: S1) => Where2_1(a, b, c)
      case (a: B2 @unchecked, _: S2, b: V2 @unchecked, _: S2, c: V2 @unchecked, _: S2) => Where2_2(a, b, c)
      case (a, _, b, _, c, _) => throw new Exception(s"Cannot make Where node for $a, $b, $c")
    }
  }

    def add(g1: ValueExpr[_ <: S], g2: ValueExpr[_ <: S]): ValueExpr[_ <: S] = {
      (g1, g1.shape, g2, g2.shape) match {
        // instead of a.asInstanceOf[N0]
        case (a: V0 @unchecked, _: S0, b: V0 @unchecked, _: S0) => a  + b
        case (a: V0 @unchecked, _: S0, b: V1 @unchecked, _: S1) => a :+ b
        case (a: V0 @unchecked, _: S0, b: V2 @unchecked, _: S2) => a :+ b
        case (a: V1 @unchecked, _: S1, b: V0 @unchecked, _: S0) => a :+ b
        case (a: V1 @unchecked, _: S1, b: V1 @unchecked, _: S1) => a  + b
        //case (a: V1 @unchecked, _: S1, b: V2 @unchecked, _: S2) => a :+ b
        case (a: V2 @unchecked, _: S2, b: V0 @unchecked, _: S0) => a :+ b
        //case (a: V2 @unchecked, _: S2, b: V1 @unchecked, _: S1) => a :+ b
        case (a: V2 @unchecked, _: S2, b: V2 @unchecked, _: S2) => a  + b
      }
    }

class GradBuilder[G <: Nat] {

  type K = ValueExpr[_ <: Nat]
  type V = ValueExpr[G]

  val builder = mutable.Map.empty[K, V]

  def +=(x: (K, V)): Unit = {
    val g = builder.get(x._1) match {
      case Some(v) => v + x._2
      case None    => x._2
    }
    builder.update(x._1, g)
  }

  def result(): Grad[G] = new Grad[G](builder.toMap[K, V])

}

  */
