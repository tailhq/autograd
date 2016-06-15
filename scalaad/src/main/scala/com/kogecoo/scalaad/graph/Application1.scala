package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{UnaryExpandOp, UnaryFoldOp, UnaryOp}
import shapeless.Nat.{_1, _2}
import shapeless.{Nat, Succ}


/**
  * represents applying unary operation, which takes 1 Expr as argument.
  *
  * @tparam N type of shape for output Expr
  * @tparam I type of shape for argument Expr
  */
abstract class Application1[N <: Nat, I <: Nat] extends V[N] {

  def shape: Shape[N]

  def v: V[I]

}


case class Apply1[N <: Nat](v: V[N], op: UnaryOp) extends Application1[N, N] {

  def shape: Shape[N] = v.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    v._forward[W, O](wrt) :* op.deriv[N](v)
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = {
    v._reverse[G](adj :* op.deriv[N](v))
  }

}


@throws[Exception]
case class Fold1[N <: Nat](v: V[Succ[N]], op: UnaryFoldOp, axis: Int) extends Application1[N, Succ[N]] {

  if (v.shape.order <= 0)       throw new Exception(s"Invalid ValueExpr order ${v.shape.order} for Fold1_-. It must be > 0.")
  if (v.shape.order - 1 < axis) throw new Exception(s"Invalid axis $axis indication for $v")

  def shape: Shape[N] = v.shape.shrink(List(axis))

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val fv = v._forward[W, Succ[O]](wrt)
    val dv = op.deriv(v)
    Fold1(fv :* dv, op, axis)
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = {
    val dv = op.deriv[Succ[N]](v)
    v._reverse[G](adj :* Fold1[N](dv, op, axis))
  }

}


case class Expand1[N <: Nat, I <: Nat](v: V[I], op: UnaryExpandOp, s: Shape[_1]) extends Application1[N, I] {

  type I_ <: Nat

  def shape: Shape[N] = v.shape.extend(s)

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val fv = v._forward[W, I_](wrt)
    val dv = op.deriv(v)
    Expand1[O, I_](fv :* dv, op, s)
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = {
    val dv = op.deriv[I](v)
    v._reverse[G](adj :* Expand1[N, I](dv, op, s))
  }

}

