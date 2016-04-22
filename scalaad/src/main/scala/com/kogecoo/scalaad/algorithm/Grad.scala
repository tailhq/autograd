package com.kogecoo.scalaad.algorithm

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.op.{Where0_0, Where0_1, Where0_2, Where1_1, Where1_2, Where2_2}

import Predef.{any2stringadd => _, _}
import scala.language.higherKinds


class Grad(val grad: Map[ValueExpr[_ <: Shape], ValueExpr[_ <: Shape]]) {

  // FIXME: These are failed when grad made by order-n node with order-(m+k) adjoint.
  //def apply(node: N0): Option[N0] = grad.get(node).map(_.asInstanceOf[N0])
  //def apply(node: N1)(implicit d: DummyImplicit): Option[N1] = grad.get(node).map(_.asInstanceOf[N1])
  //def apply(node: N2)(implicit d: DummyImplicit, d2: DummyImplicit): Option[N2] = grad.get(node).map(_.asInstanceOf[N2])

  def size: Int = grad.size
  // we cannot call eval with node which extracted by higher kind method.
  def apply(node: ValueExpr[_ <: Shape]): Option[ValueExpr[_ <: Shape]] = grad.get(node)

}

object Grad {

  private[this] type S = Shape

  def apply(node: ValueExpr[_ <: S], grad: ValueExpr[_ <: S]): Grad = new Grad(Map(node -> grad))

  def empty: Grad = new Grad(Map.empty[ValueExpr[_ <: S], ValueExpr[_ <: S]])

  def where(cond: BooleanExpr[_ <: S], g1: Grad, g2: Grad): Grad = {
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
      case (a: B0 @unchecked, _: S0, b: N0 @unchecked, _: S0, c: N0 @unchecked, _: S0) => Where0_0(a, b, c)
      case (a: B0 @unchecked, _: S0, b: N1 @unchecked, _: S1, c: N1 @unchecked, _: S1) => Where0_1(a, b, c)
      case (a: B0 @unchecked, _: S0, b: N2 @unchecked, _: S2, c: N2 @unchecked, _: S2) => Where0_2(a, b, c)
      //case (a: B1 @unchecked, _: S1, b: N0 @unchecked, _: S0, c: N0 @unchecked, _: S0) => Where1_0(a, b, c)
      case (a: B1 @unchecked, _: S1, b: N1 @unchecked, _: S1, c: N1 @unchecked, _: S1) => Where1_1(a, b, c)
      case (a: B1 @unchecked, _: S1, b: N2 @unchecked, _: S2, c: N2 @unchecked, _: S2) => Where1_2(a, b, c)
      //case (a: B2 @unchecked, _: S2, b: N0 @unchecked, _: S0, c: N0 @unchecked, _: S0) => Where2_0(a, b, c)
      //case (a: B2 @unchecked, _: S2, b: N1 @unchecked, _: S1, c: N1 @unchecked, _: S1) => Where2_1(a, b, c)
      case (a: B2 @unchecked, _: S2, b: N2 @unchecked, _: S2, c: N2 @unchecked, _: S2) => Where2_2(a, b, c)
      case (a, _, b, _, c, _) => throw new Exception(s"Cannot make Where node for $a, $b, $c")
    }
  }

  implicit class GradOp(val self: Grad) extends AnyVal {
    def ++(other: Grad): Grad = {
      val keys = self.grad.keySet | other.grad.keySet
      val (m: Map[ValueExpr[_ <: S], ValueExpr[_ <: S]]) = (for {
        k <- keys
        s =  self.grad.get(k)
        o =  other.grad.get(k)
        if s.isDefined || o.isDefined
      } yield {
        if (s.isDefined && o.isDefined) {
          (k, add(s.get, o.get))
        } else if (s.isDefined) {
          (k, s.get)
        } else {
          (k, o.get)
        }
      }).toMap
      new Grad(m)
    }

    def add(g1: ValueExpr[_ <: S], g2: ValueExpr[_ <: S]): ValueExpr[_ <: S] = {
      (g1, g1.shape, g2, g2.shape) match {
        // instead of a.asInstanceOf[N0]
        case (a: N0 @unchecked, _: S0, b: N0 @unchecked, _: S0) => a  + b
        case (a: N0 @unchecked, _: S0, b: N1 @unchecked, _: S1) => a :+ b
        case (a: N0 @unchecked, _: S0, b: N2 @unchecked, _: S2) => a :+ b
        case (a: N1 @unchecked, _: S1, b: N0 @unchecked, _: S0) => a :+ b
        case (a: N1 @unchecked, _: S1, b: N1 @unchecked, _: S1) => a  + b
        case (a: N1 @unchecked, _: S1, b: N2 @unchecked, _: S2) => a :+ b
        case (a: N2 @unchecked, _: S2, b: N0 @unchecked, _: S0) => a :+ b
        case (a: N2 @unchecked, _: S2, b: N1 @unchecked, _: S1) => a :+ b
        case (a: N2 @unchecked, _: S2, b: N2 @unchecked, _: S2) => a  + b
      }
    }
  }

}


