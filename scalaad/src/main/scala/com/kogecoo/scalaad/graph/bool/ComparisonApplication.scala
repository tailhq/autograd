package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.graph.{VE, VE0, ValueExpr}
import com.kogecoo.scalaad.op.Op00C
import com.kogecoo.scalaad.{S0, Shape}


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
trait CommonShapedComparisonApplication2[S <: Shape] extends ComparisonApplication2[S, S, S] {
  override val shape: S = l.shape
  override def l: ValueExpr[S]
  override def r: ValueExpr[S]
}


// Binary ComparisonApplication

case class Apply2C[S <: Shape](l: VE[S], r: VE[S], op: Op00C) extends CommonShapedComparisonApplication2[S]

case class ElementwiseLeftC[S <: Shape](l: VE[S], r: VE0, op: Op00C) extends LeftShapedComparisonApplication2[S, S0]

case class ElementwiseRightC[S <: Shape](l: VE0, r: VE[S], op: Op00C) extends RightShapedComparisonApplication2[S0, S]

