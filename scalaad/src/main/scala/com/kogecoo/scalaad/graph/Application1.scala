package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{UnaryFoldOp, UnaryOp}
import shapeless.Nat


/**
  * represents applying unary operation, which takes 1 Expr as argument.
  *
  * @tparam N type of shape for output Expr
  * @tparam I type of shape for argument Expr
  */
trait Application1[N <: Nat, I <: Nat] extends ValueExpr[N] {

  def shape: Shape[N]

  def v: ValueExpr[I]

}


// Unary Application

case class Apply1[N <: Nat](v: V[N], op: UnaryOp) extends Application1[N, N] {

  def shape: Shape[N] = v.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    v._forward[W, O](wrt) :* op.deriv[N](v)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = {
    v._reverse[G](g :* op.deriv[N](v), builder)
  }

}


case class Fold1[N <: Nat, I <: Nat](v: V[I], override val shape: Shape[N], op: UnaryFoldOp) extends Application1[N, I] {

  type I_ <: Nat

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Fold1(v._forward[W, I_](wrt), shape.extend(v.shape).asInstanceOf[Shape[O]], op)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }

}


case class Expand1[N <: Nat, I <: Nat](v: V[I], shape: Shape[N]) extends Application1[N, I] {

  type I_ <: Nat

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Expand1(v._forward[W, I_](wrt), shape.extend(v.shape).asInstanceOf[Shape[O]])
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }

}
