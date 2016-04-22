package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import shapeless.Nat


abstract class WhereBase[N <: Nat, C <: Nat, L <: Nat, R <: Nat](cond: B[C], l: V[L], r: V[R]) extends ValueExpr[N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Unsafe.where(cond, l._forward[W, O](wrt), r._forward[W, O](wrt))
  }

  def _reverse[G <: Nat](adj: ValueExpr[G]): Grad[G] = {
    val lg: Grad[G] = l._reverse(adj)
    val rg: Grad[G] = r._reverse(adj)
    interpolateToGrad(lg, rg)
  }

  protected[this] def interpolateToGrad[G <: Nat](lg: Grad[G], rg: Grad[G]): Grad[G] = {
    val builder = Map.newBuilder[ValueExpr[_ <: Nat], ValueExpr[G]]

    (lg.m.keySet | rg.m.keySet).foreach { k =>
      (lg.m.get(k), rg.m.get(k)) match {
        case (Some(a), Some(b)) => builder += ((k, Unsafe.where(cond, a, b)))
        case (Some(a), None)    => builder += ((k, Unsafe.if_(cond, a)))
        case (None,    Some(b)) => builder += ((k, Unsafe.notif(cond, b)))
        case (None,    None)    => ()
      }
    }
    new Grad[G](builder.result())
  }

}


abstract class IfBase[N <: Nat, C <: Nat](cond: B[C], v: V[N]) extends ValueExpr[N] {

  def shape: Shape[N] = v.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Unsafe.if_(cond, v._forward[W, O](wrt))
  }

  def _reverse[G <: Nat](adj: ValueExpr[G]): Grad[G] = {
    val builder = Map.newBuilder[ValueExpr[_ <: Nat], ValueExpr[G]]
    v._reverse(adj).m.foreach { case (key, value) => builder += ((key, Unsafe.if_(cond, value))) }
    new Grad[G](builder.result())
  }

}


abstract class NotIfBase[N <: Nat, C <: Nat](cond: B[C], v: V[N]) extends ValueExpr[N] {

  def shape: Shape[N] = v.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Unsafe.notif(cond, v._forward[W, O](wrt))
  }

  def _reverse[G <: Nat](adj: ValueExpr[G]): Grad[G] = {
    val builder = Map.newBuilder[ValueExpr[_ <: Nat], ValueExpr[G]]
    v._reverse(adj).m.foreach { case (key, value) => builder += ((key, Unsafe.notif(cond, value))) }
    new Grad[G](builder.result())
  }

}


@throws[Exception]
case class Where[N <:  Nat](cond: B[N], l: V[N], r: V[N]) extends WhereBase[N, N, N, N](cond, l, r) {

  if (cond.shape == l.shape && l.shape == r.shape)
    throw new Exception(s"Shapes of the cond (${cond.shape}), left (${l.shape}) and the right (${r.shape}) ValueExpr must be equivalent.")

  def shape: Shape[N] = cond.shape

}


@throws[Exception]
case class AsymmetricWhere[N <:  Nat, C <: Nat](cond: B[C], l: V[N], r: V[N]) extends WhereBase[N, C, N, N](cond, l, r) {

  if (l.shape == r.shape)
    throw new Exception(s"Shapes of the l (${l.shape}) and the r (${r.shape}) ValueExpr must be equivalent.")

  if (cond.shape.order < l.shape.order)
    throw new Exception(s"Orders of the l (${l.shape}) and the r (${r.shape}) must be larger than cond (${cond.shape.order}).")

  if (!l.shape.hasSamePrefixWith(cond.shape)) {
    val expectedShape = l.shape.shrink(r.shape.underlying.indices.toList.drop(cond.shape.order))
    val msg = s"Shapes of the cond (${cond.shape}) and l (${l.shape}) / r (${r.shape}) are not aligned." +
              s" The shape of v must be $expectedShape."
    throw new Exception(msg)
  }

  def shape: Shape[N] = l.shape

}


@throws[Exception]
case class If[N <: Nat](cond: B[N], v: V[N]) extends IfBase[N, N](cond, v) {

  if (cond.shape == v.shape)
    throw new Exception(s"Shapes of the cond (${cond.shape}) and v (${v.shape}) ValueExpr must be equivalent.")

}


@throws[Exception]
case class AsymmetricIf[N <: Nat, C <: Nat](cond: B[C], v: V[N]) extends IfBase[N, C](cond, v) {

  if (!v.shape.hasSamePrefixWith(cond.shape)) {
    val expectedShape = v.shape.shrink(v.shape.underlying.indices.toList.drop(cond.shape.order))
    val msg = s"Shapes of the cond (${cond.shape}) and  v (${v.shape}) are not aligned." +
              s" The shape of v must be $expectedShape."
    throw new Exception(msg)
  }

}


@throws[Exception]
case class NotIf[N <: Nat](cond: B[N], v: V[N]) extends NotIfBase[N, N](cond, v) {

   if (cond.shape == v.shape)
    throw new Exception(s"Shapes of the cond (${cond.shape}) and v (${v.shape}) ValueExpr must be equivalent.")

}


@throws[Exception]
case class AsymmetricNotIf[N <: Nat, C <: Nat](cond: B[C], v: V[N]) extends NotIfBase[N, C](cond, v) {

  if (!v.shape.hasSamePrefixWith(cond.shape)) {
    val expectedShape = v.shape.shrink(v.shape.underlying.indices.toList.drop(cond.shape.order))
    val msg = s"Shapes of the cond (${cond.shape}) and  v (${v.shape}) are not aligned." +
              s" The shape of v must be $expectedShape."
    throw new Exception(msg)
  }

}
