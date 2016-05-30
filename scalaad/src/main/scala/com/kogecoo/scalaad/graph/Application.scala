package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.op.{BinaryOp, Op0, Op00, UnaryOp}
import com.kogecoo.scalaad.{S0, Shape, Shape0}


/**
  * represents applying binary operation, which takes 2 Exprs arguments.
  *
  * @tparam S a shape of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right Expr
  */
trait Application2[S <: Shape, L <: Shape, R <: Shape] extends ValueExpr[S] {
  val shape: S
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its left and output Expr are the same type of shape.
  *
  * @tparam L a shape of left and output Expr
  * @tparam R a shape of right Expr
  */
trait LeftShapedApplication2[L <: Shape, R <: Shape] extends Application2[L, L, R] {
  val shape: L = l.shape
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its right and output Expr are the same type of shape.
  *
  * @tparam L a shape of left Expr
  * @tparam R a shape of right and output Expr
  */
trait RightShapedApplication2[L <: Shape, R <: Shape] extends Application2[R, L, R] {
  val shape: R = r.shape
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its left, right and output Expr are the same type of shape.
  *
  * @tparam S type of shape for left, right and output Expr
  */
trait CommonShapedApplication2[S <: Shape] extends Application2[S, S, S] {
  val shape: S = l.shape
  def l: ValueExpr[S]
  def r: ValueExpr[S]
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
  *
  * @tparam O type of shape for output Expr
  * @tparam S type of shape for argument Expr
  */
trait Application1[O <: Shape, S <: Shape] extends ValueExpr[O] {
  val shape: O
  def v: ValueExpr[S]
}

/**
  * specialized Application1, its input and output Expr are the same type of shape.
  *
  * @tparam S type of shape for left, right and output Expr
  */
trait CommonShapedApplication1[S <: Shape] extends Application1[S, S] {
  val shape: S = v.shape
  def v: ValueExpr[S]
}


// Unary Application

case class Apply1[S <: Shape](v: ValueExpr[S], op: Op0) extends CommonShapedApplication1[S]

case class Fold1[SI1 <: Shape](v: ValueExpr[SI1], op: UnaryOp[S0, SI1]) extends Application1[S0, SI1] { def shape: S0 = Shape0() }

case class Fill[SO <: Shape](v: ValueExpr[S0], shape: SO) extends Application1[SO, S0]


// Binary Application

case class Apply2[S <: Shape](l: ValueExpr[S], r: ValueExpr[S], op: Op00) extends CommonShapedApplication2[S]

case class ElementwiseLeft[S <: Shape](l: ValueExpr[S], r: V0, op: Op00) extends LeftShapedApplication2[S, S0]

case class ElementwiseRight[S <: Shape](l: V0, r: ValueExpr[S], op: Op00) extends RightShapedApplication2[S0, S]

case class Fold2[SI1 <: Shape, SI2 <: Shape](l: ValueExpr[SI1], r: ValueExpr[SI2], op: BinaryOp[S0, SI1, SI2]) extends Application2[S0, SI1, SI2] { def shape: S0 = Shape0() }

/*
case class RowwiseRight(l: V1, r: V2, op: Op00) extends RightShapedApplication2[S1, S2]

case class RowwiseLeft(l: V2, r: V1, op: Op00) extends LeftShapedApplication2[S2, S1]

case class ColumnwiseLeft(l: V2, r: V1, op: Op00) extends RightShapedApplication2[S2, S1]

case class ColumnwiseRight(l: V1, r: V2, op: Op00) extends RightShapedApplication2[S1, S2]
*/
