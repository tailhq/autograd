package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.B
import com.kogecoo.scalaad.op.bool.{BinaryBooleanOp, UnaryBooleanExpandOp, UnaryBooleanOp}
import shapeless.Nat


// TODO: code-sharing with Application

/**
  * represents applying binary operation, which takes 2 Exprs arguments.
 *
  * @tparam N a shape of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right Expr
  */
trait BooleanApplication2[N <: Nat, L <: Nat, R <: Nat] extends B[N] {

  def l: B[L]

  def r: B[R]
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
 *
  * @tparam N type of shape for output Expr
  * @tparam I type of shape for argument Expr
  */
trait BooleanApplication1[N <: Nat, I <: Nat] extends B[N] {

  def v: B[I]

}

// Unary BooleanApplication

case class Apply1B[N <: Nat](v: B[N], op: UnaryBooleanOp) extends BooleanApplication1[N, N] {

  def shape: Shape[N] = v.shape

}


case class Apply2B[N <: Nat](l: B[N], r: B[N], op: BinaryBooleanOp) extends BooleanApplication2[N, N, N] {

  def shape: Shape[N] = l.shape
}


case class ApplyLeftB[L <: Nat, R <: Nat](l: B[L], r: B[R], op: BinaryBooleanOp) extends BooleanApplication2[L, L, R] {

  def shape: Shape[L] = l.shape

}


case class ApplyRightB[L <: Nat, R <: Nat](l: B[L], r: B[R], op: BinaryBooleanOp) extends BooleanApplication2[R, L, R] {

  def shape: Shape[R] = r.shape

}


case class Expand1B[N <: Nat, L <: Nat, R <: Nat](v: B[L], s: Shape[R], op: UnaryBooleanExpandOp) extends BooleanApplication1[N, L] {

  def shape: Shape[N] = v.shape.extend(s)

}
