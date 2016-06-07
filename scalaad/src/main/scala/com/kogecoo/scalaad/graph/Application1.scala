package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{BinaryOp, Mul, UnaryOp}
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

case class Apply1[N <: Nat](v: VE[N], op: UnaryOp) extends CommonShapedApplication1[N] {

  def _forward[W <: Nat, O <: Nat](wrt: VE[W]): VE[O] = {
    ElementwiseLeft(v._forward(wrt), op.deriv[N](v), Mul)
  }

}

/*
case class Fold1[SI1 <: Nat](v: VE[SI1], op: UnaryOp) extends Application1[S0, SI1] {

  def shape: Shape[S0] = Shape0()

}


case class Fill[SO <: Nat](v: VE0, shape: SO) extends Application1[SO, S0] {

}
*/

