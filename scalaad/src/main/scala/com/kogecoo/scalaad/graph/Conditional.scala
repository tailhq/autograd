package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import shapeless.Nat


abstract class Where[N <: Nat, C <: Nat, L <: Nat, R <: Nat](cond: B[C], l: V[L], r: V[R]) extends ValueExpr[N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Where(cond, l._forward[W, O](wrt), r._forward[W, O](wrt))
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
        case (Some(a), Some(b)) => builder += ((k, Where(cond, a, b)))
        case (Some(a), None)    => builder += ((k, Where(cond, a, Zero[G](a.shape))))
        case (None,    Some(b)) => builder += ((k, Where(cond, Zero[G](b.shape), b)))
        case (None,    None)    => ()
      }
    }
    new Grad[G](builder.result())
  }

}


object Where {

  def apply[O <: Nat, C <: Nat, X <: Nat, Y <: Nat](cond: B[C], x: V[X], y: V[Y]): V[O] = {
    (cond.shape.order, x.shape.order, y.shape.order) match {
      case (co, xo, yo) if co == xo && xo == yo => ElementwiseWhere[O](cond.asInstanceOf[B[O]], x.asInstanceOf[V[O]], y.asInstanceOf[V[O]])
      case (co, xo, yo) if co <  xo && xo == yo => BroadcastWhere[O, C](cond, x.asInstanceOf[V[O]], y.asInstanceOf[V[O]])
      case (co, xo, yo)                         => throw new Exception(s"Illegal shape order combination for Where operator ($co, $xo, $yo)")
    }
  }

}


@throws[Exception]
case class ElementwiseWhere[N <:  Nat](cond: B[N], l: V[N], r: V[N]) extends Where[N, N, N, N](cond, l, r) {

  if (cond.shape == l.shape && l.shape == r.shape)
    throw new Exception(s"Shapes of the cond (${cond.shape}), left (${l.shape}) and the right (${r.shape}) ValueExpr must be equivalent.")

  def shape: Shape[N] = cond.shape

}


@throws[Exception]
case class BroadcastWhere[N <:  Nat, C <: Nat](cond: B[C], l: V[N], r: V[N]) extends Where[N, C, N, N](cond, l, r) {

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
