package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.BE
import com.kogecoo.scalaad.op.bool.{AsymmetricLeftBinaryBooleanOp, AsymmetricRightBinaryBooleanOp, BinaryBooleanOp, UnaryBooleanOp}
import shapeless.Nat


// TODO: code-sharing with Application

/**
  * represents applying binary operation, which takes 2 Exprs arguments.
 *
  * @tparam N a shape of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right Expr
  */
trait BooleanApplication2[N <: Nat, L <: Nat, R <: Nat] extends BooleanExpr[N] {
  val shape: Shape[N]
  def l: BooleanExpr[L]
  def r: BooleanExpr[R]
}

/**
  * specialized BooleanApplication2, its left and output Expr are the same type of shape.
 *
  * @tparam L a shape of left and output Expr
  * @tparam R a shape of right Expr
  */
trait LeftShapedBooleanApplication2[L <: Nat, R <: Nat] extends BooleanApplication2[L, L, R] {
  override val shape: Shape[L] = l.shape
  override def l: BooleanExpr[L]
  override def r: BooleanExpr[R]
}

/**
  * specialized BooleanApplication2, its right and output Expr are the same type of shape.
 *
  * @tparam L a shape of left Expr
  * @tparam R a shape of right and output Expr
  */
trait RightShapedBooleanApplication2[L <: Nat, R <: Nat] extends BooleanApplication2[R, L, R] {
  override val shape: Shape[R] = r.shape
  override def l: BooleanExpr[L]
  override def r: BooleanExpr[R]
}

/**
  * specialized BooleanApplication2, its left, right and output Expr are the same type of shape.
 *
  * @tparam N type of shape for left, right and output Expr
  */
trait CommonShapedBooleanApplication2[N <: Nat] extends BooleanApplication2[N, N, N] {
  override val shape: Shape[N] = l.shape
  override def l: BooleanExpr[N]
  override def r: BooleanExpr[N]
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
 *
  * @tparam O type of shape for output Expr
  * @tparam I type of shape for argument Expr
  */
trait BooleanApplication1[O <: Nat, I <: Nat] extends BooleanExpr[O] {
  val shape: Shape[O]
  def v: BooleanExpr[I]
}

/**
  * specialized BooleanApplication1, its input and output Expr are the same type of shape.
 *
  * @tparam N type of shape for left, right and output Expr
  */
trait CommonShapedBooleanApplication1[N <: Nat] extends BooleanApplication1[N, N] {
  override val shape: Shape[N] = v.shape

  override def v: BooleanExpr[N]
}


// Unary BooleanApplication

case class Apply1B[S <: Nat](v: BooleanExpr[S], op: UnaryBooleanOp) extends CommonShapedBooleanApplication1[S]

/*case class Fold1B[SI1 <: Nat](v: BooleanExpr[SI1], op: UnaryBooleanOp) extends BooleanApplication1[S0, SI1] { val shape: S0 = Shape0() }

case class FillB[SO <: Nat](v: BE0, shape: SO) extends BooleanApplication1[SO, S0]
*/


case class Apply2B[N <: Nat](l: BE[N], r: BE[N], op: BinaryBooleanOp) extends CommonShapedBooleanApplication2[N]

case class ElementwiseLeftB[L <: Nat, R <: Nat](l: BE[L], r: BE[R], op: AsymmetricLeftBinaryBooleanOp) extends LeftShapedBooleanApplication2[L, R]

case class ElementwiseRightB[L <: Nat, R <: Nat](l: BE[L], r: BE[R], op: AsymmetricRightBinaryBooleanOp) extends RightShapedBooleanApplication2[L, R]

//case class Fold2B[SI1 <: Nat, SI2 <: Nat](l: BE[SI1], r: BE[SI2], op: BinaryBooleanOp[S0, SI1, SI2]) extends BooleanApplication2[S0, SI1, SI2] { val shape: S0 = Shape0() }

