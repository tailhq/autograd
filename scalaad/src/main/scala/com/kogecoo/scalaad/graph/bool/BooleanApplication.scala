package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.graph.{BE, BE0}
import com.kogecoo.scalaad.op.bool.{BinaryBooleanOp, UnaryBooleanOp}
import com.kogecoo.scalaad.op.{Op00B, Op0B}
import com.kogecoo.scalaad.{S0, Shape, Shape0}


// TODO: code-sharing with Application

/**
  * represents applying binary operation, which takes 2 Exprs arguments.
 *
  * @tparam S a shape of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right Expr
  */
trait BooleanApplication2[S <: Shape, L <: Shape, R <: Shape] extends BooleanExpr[S] {
  val shape: S
  def l: BooleanExpr[L]
  def r: BooleanExpr[R]
}

/**
  * specialized BooleanApplication2, its left and output Expr are the same type of shape.
 *
  * @tparam L a shape of left and output Expr
  * @tparam R a shape of right Expr
  */
trait LeftShapedBooleanApplication2[L <: Shape, R <: Shape] extends BooleanApplication2[L, L, R] {
  override val shape: L = l.shape
  override def l: BooleanExpr[L]
  override def r: BooleanExpr[R]
}

/**
  * specialized BooleanApplication2, its right and output Expr are the same type of shape.
 *
  * @tparam L a shape of left Expr
  * @tparam R a shape of right and output Expr
  */
trait RightShapedBooleanApplication2[L <: Shape, R <: Shape] extends BooleanApplication2[R, L, R] {
  override val shape: R = r.shape
  override def l: BooleanExpr[L]
  override def r: BooleanExpr[R]
}

/**
  * specialized BooleanApplication2, its left, right and output Expr are the same type of shape.
 *
  * @tparam S type of shape for left, right and output Expr
  */
trait CommonShapedBooleanApplication2[S <: Shape] extends BooleanApplication2[S, S, S] {
  override val shape: S = l.shape
  override def l: BooleanExpr[S]
  override def r: BooleanExpr[S]
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
 *
  * @tparam O type of shape for output Expr
  * @tparam S type of shape for argument Expr
  */
trait BooleanApplication1[O <: Shape, S <: Shape] extends BooleanExpr[O] {
  val shape: O
  def v: BooleanExpr[S]
}

/**
  * specialized BooleanApplication1, its input and output Expr are the same type of shape.
 *
  * @tparam S type of shape for left, right and output Expr
  */
trait CommonShapedBooleanApplication1[S <: Shape] extends BooleanApplication1[S, S] {
  override val shape: S = v.shape

  override def v: BooleanExpr[S]
}


// Unary BooleanApplication

case class Apply1B[S <: Shape](v: BooleanExpr[S], op: Op0B) extends CommonShapedBooleanApplication1[S]

case class Fold1B[SI1 <: Shape](v: BooleanExpr[SI1], op: UnaryBooleanOp[S0, SI1]) extends BooleanApplication1[S0, SI1] { val shape: S0 = Shape0() }

case class FillB[SO <: Shape](v: BE0, shape: SO) extends BooleanApplication1[SO, S0]


case class Apply2B[S <: Shape](l: BE[S], r: BE[S], op: Op00B) extends CommonShapedBooleanApplication2[S]

case class ElementwiseLeftB[S <: Shape](l: BE[S], r: BE0, op: Op00B) extends LeftShapedBooleanApplication2[S, S0]

case class ElementwiseRightB[S <: Shape](l: BE0, r: BE[S], op: Op00B) extends RightShapedBooleanApplication2[S0, S]

case class Fold2B[SI1 <: Shape, SI2 <: Shape](l: BE[SI1], r: BE[SI2], op: BinaryBooleanOp[S0, SI1, SI2]) extends BooleanApplication2[S0, SI1, SI2] { val shape: S0 = Shape0() }

