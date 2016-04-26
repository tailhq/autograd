package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.op.{BooleanOp0, BooleanOp00, Op00}
import com.kogecoo.scalaad.{Shape, Shape0, Shape1, Shape2}


// TODO: code-sharing with Application

/**
  * represents applying binary operation, which takes 2 Exprs arguments.
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
  * @tparam S type of shape for left, right and output Expr
  */
trait SameShapedBooleanApplication2[S <: Shape] extends BooleanApplication2[S, S, S] {
  override val shape: S = l.shape
  override def l: BooleanExpr[S]
  override def r: BooleanExpr[S]
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
  * @tparam O type of shape for output Expr
  * @tparam S type of shape for argument Expr
  */
trait BooleanApplication1[O <: Shape, S <: Shape] extends BooleanExpr[O] {
  val shape: O
  def v: BooleanExpr[S]
}

/**
  * specialized BooleanApplication1, its input and output Expr are the same type of shape.
  * @tparam S type of shape for left, right and output Expr
  */
trait SameShapedBooleanApplication1[S <: Shape] extends BooleanApplication1[S, S] {
  override val shape: S = v.shape

  override def v: BooleanExpr[S]
}


// Unary BooleanApplication

case class Apply0B(v: B0, op: BooleanOp0) extends SameShapedBooleanApplication1[S0]


case class Elementwise1B(v: B1, op: BooleanOp0) extends SameShapedBooleanApplication1[S1]

case class Elementwise2B(v: B2, op: BooleanOp0) extends SameShapedBooleanApplication1[S2]


case class Broadcast1B(v: B1, op: BooleanOp0) extends SameShapedBooleanApplication1[S1]

case class Broadcast2B(v: B2, op: BooleanOp0) extends SameShapedBooleanApplication1[S2]


case class Fold1B(v: B1, op: BooleanOp00) extends BooleanApplication1[S0, S1] { val shape: S0 = Shape0() }

case class Fold2B(v: B2, op: BooleanOp00) extends BooleanApplication1[S0, S2] { val shape: S0 = Shape0() }


case class FoldRowwise2B(v: B2, op: BooleanOp00) extends BooleanApplication1[S1, S2] { val shape: S1 = Shape1(v.shape._1) }

case class FoldColumnwise2B(v: B2, op: BooleanOp00) extends BooleanApplication1[S1, S2] { val shape: S1 = Shape1(v.shape._2) }


// Experimental Unary BooleanApplication

case class VecFillB(v: B0, shape: S1) extends BooleanApplication1[S1, S0]

case class MatFillB(v: B0, shape: S2) extends BooleanApplication1[S2, S0]


case class MatFillAcrossRowB(v: B1, numColumns: Int) extends BooleanApplication1[S2, S1] { val shape: S2 = Shape2(v.shape._1, numColumns) }

case class MatFillAcrossColumnB(v: B1, numRows: Int) extends BooleanApplication1[S2, S1] { val shape: S2 = Shape2(numRows, v.shape._1) }


// Binary BooleanApplication

case class Apply00B(l: B0, r: B0, op: BooleanOp00) extends SameShapedBooleanApplication2[S0]


case class Elementwise11B(l: B1, r: B1, op: BooleanOp00) extends SameShapedBooleanApplication2[S1]

case class Elementwise22B(l: B2, r: B2, op: BooleanOp00) extends SameShapedBooleanApplication2[S2]


case class Broadcast01B(l: B0, r: B1, op: BooleanOp00) extends RightShapedBooleanApplication2[S0, S1]

case class Broadcast02B(l: B0, r: B2, op: BooleanOp00) extends RightShapedBooleanApplication2[S0, S2]

case class Broadcast10B(l: B1, r: B0, op: BooleanOp00) extends LeftShapedBooleanApplication2[S1, S0]

case class Broadcast20B(l: B2, r: B0, op: BooleanOp00) extends LeftShapedBooleanApplication2[S2, S0]


case class Rowwise12B(l: B1, r: B2, op: BooleanOp00) extends RightShapedBooleanApplication2[S1, S2]

case class Rowwise21B(l: B2, r: B1, op: BooleanOp00) extends LeftShapedBooleanApplication2[S2, S1]


case class Columnwise12B(l: B1, r: B2, op: BooleanOp00) extends RightShapedBooleanApplication2[S1, S2]

case class Columnwise21B(l: B2, r: B1, op: BooleanOp00) extends RightShapedBooleanApplication2[S2, S1]


