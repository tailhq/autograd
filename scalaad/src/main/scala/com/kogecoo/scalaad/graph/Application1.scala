package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{Mul, UnaryFoldOp, UnaryOp}
import shapeless.Nat


/**
  * represents applying unary operation, which takes 1 Expr as argument.
  *
  * @tparam O type of shape for output Expr
  * @tparam N type of shape for argument Expr
  */
trait Application1[O <: Nat, N <: Nat] extends ValueExpr[O] {

  def shape: Shape[O]

  def v: ValueExpr[N]

}

/**
  * specialized Application1, its input and output Expr are the same type of shape.
  *
  * @tparam N type of shape for left, right and output Expr
  */
trait CommonShapedApplication1[N <: Nat] extends Application1[N, N] {
  def shape: Shape[N] = v.shape
  def v: ValueExpr[N]
}


// Unary Application

case class Apply1[N <: Nat](v: V[N], op: UnaryOp) extends CommonShapedApplication1[N] {


  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    ElementwiseLeft(v._forward[W, O](wrt), op.deriv[N](v), Mul)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = {
    v._reverse[G](g * op.deriv[N](v), builder)
  }

}

case class Fold1[N <: Nat, I <: Nat](v: V[I], override val shape: Shape[N], op: UnaryFoldOp) extends Application1[N, I] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = { }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }
}


case class Expand1[N <: Nat, I <: Nat](v: V[I], shape: Shape[N]) extends Application1[N, I] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = { }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }

}

