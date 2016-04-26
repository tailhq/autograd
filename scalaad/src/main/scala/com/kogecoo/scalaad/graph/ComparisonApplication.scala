package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{BooleanOp00, ComparisonOp00}


// TODO: code-sharing with Application

/**
  * represents applying binary comparing operation, which takes 2 Exprs arguments.
  *
  * @tparam S a shape of this  BooleanExpr
  * @tparam L a shape of left  ValueExpr
  * @tparam R a shape of right ValueExpr
  */
trait ComparisonApplication2[S <: Shape, L <: Shape, R <: Shape] extends BooleanExpr[S] {
  val shape: S
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized ComparisonApplication2, its left ValueExpr and output BooleanExpr are the same type of shape.
  *
  * @tparam L a shape of left ValueExpr and output BooleanExpr
  * @tparam R a shape of right ValueExpr
  */
trait LeftShapedComparisonApplication2[L <: Shape, R <: Shape] extends ComparisonApplication2[L, L, R] {
  override val shape: L = l.shape
  override def l: ValueExpr[L]
  override def r: ValueExpr[R]
}

/**
  * specialized ComparisonApplication2, its right and output Expr are the same type of shape.
  *
  * @tparam L a shape of left ValueExpr
  * @tparam R a shape of right ValueExpr and output BooleanExpr
  */
trait RightShapedComparisonApplication2[L <: Shape, R <: Shape] extends ComparisonApplication2[R, L, R] {
  override val shape: R = r.shape
  override def l: ValueExpr[L]
  override def r: ValueExpr[R]
}

/**
  * specialized ComparisonApplication2, its left, right ValueExpr and output BooleanExpr are the same type of shape.
  *
  * @tparam S type of shape for left, right and output Expr
  */
trait SameShapedComparisonApplication2[S <: Shape] extends ComparisonApplication2[S, S, S] {
  override val shape: S = l.shape
  override def l: ValueExpr[S]
  override def r: ValueExpr[S]
}

// Binary ComparisonApplication

case class Apply00C(l: V0, r: V0, op: ComparisonOp00) extends SameShapedComparisonApplication2[S0]

case class Elementwise11C(l: V1, r: V1, op: ComparisonOp00) extends SameShapedComparisonApplication2[S1]

case class Elementwise22C(l: V2, r: V2, op: ComparisonOp00) extends SameShapedComparisonApplication2[S2]


case class Broadcast01C(l: V0, r: V1, op: ComparisonOp00) extends RightShapedComparisonApplication2[S0, S1]

case class Broadcast02C(l: V0, r: V2, op: ComparisonOp00) extends RightShapedComparisonApplication2[S0, S2]

case class Broadcast10C(l: V1, r: V0, op: ComparisonOp00) extends LeftShapedComparisonApplication2[S1, S0]

case class Broadcast20C(l: V2, r: V0, op: ComparisonOp00) extends LeftShapedComparisonApplication2[S2, S0]

