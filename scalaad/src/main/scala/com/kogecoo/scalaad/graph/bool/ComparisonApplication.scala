package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.{V, ValueExpr}
import com.kogecoo.scalaad.op.bool.BinaryComparisonOp
import shapeless.Nat


// TODO: code-sharing with Application

/**
  * represents applying binary comparing operation, which takes 2 Exprs arguments.
  *
  * @tparam N a shape of this  BooleanExpr
  * @tparam L a shape of left  ValueExpr
  * @tparam R a shape of right ValueExpr
  */
trait ComparisonApplication2[N <: Nat, L <: Nat, R <: Nat] extends BooleanExpr[N] {
  val shape: Shape[N]
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized ComparisonApplication2, its left ValueExpr and output BooleanExpr are the same type of shape.
  *
  * @tparam L a shape of left ValueExpr and output BooleanExpr
  * @tparam R a shape of right ValueExpr
  */
trait LeftShapedComparisonApplication2[L <: Nat, R <: Nat] extends ComparisonApplication2[L, L, R] {
  override val shape: Shape[L] = l.shape
  override def l: ValueExpr[L]
  override def r: ValueExpr[R]
}

/**
  * specialized ComparisonApplication2, its right and output Expr are the same type of shape.
  *
  * @tparam L a shape of left ValueExpr
  * @tparam R a shape of right ValueExpr and output BooleanExpr
  */
trait RightShapedComparisonApplication2[L <: Nat, R <: Nat] extends ComparisonApplication2[R, L, R] {
  override val shape: Shape[R] = r.shape
  override def l: ValueExpr[L]
  override def r: ValueExpr[R]
}

/**
  * specialized ComparisonApplication2, its left, right ValueExpr and output BooleanExpr are the same type of shape.
  *
  * @tparam N type of shape for left, right and output Expr
  */
trait CommonShapedComparisonApplication2[N <: Nat] extends ComparisonApplication2[N, N, N] {
  override val shape: Shape[N] = l.shape
  override def l: ValueExpr[N]
  override def r: ValueExpr[N]
}


// Binary ComparisonApplication

case class Apply2C[S <: Nat](l: V[S], r: V[S], op: BinaryComparisonOp) extends CommonShapedComparisonApplication2[S]

case class ElementwiseLeftC[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryComparisonOp) extends LeftShapedComparisonApplication2[L, R]

case class ElementwiseRightC[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryComparisonOp) extends RightShapedComparisonApplication2[L, R]

