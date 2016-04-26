package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.op.{Op0, Op00}
import com.kogecoo.scalaad.{Shape, Shape0, Shape1, Shape2}


/**
  * represents applying binary operation, which takes 2 Exprs arguments.
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
  * @tparam L a shape of left and output Expr
  * @tparam R a shape of right Expr
  */
trait LeftShapedApplication2[L <: Shape, R <: Shape] extends Application2[L, L, R] {
  override val shape: L = l.shape
  override def l: ValueExpr[L]
  override def r: ValueExpr[R]
}

/**
  * specialized Application2, its right and output Expr are the same type of shape.
  * @tparam L a shape of left Expr
  * @tparam R a shape of right and output Expr
  */
trait RightShapedApplication2[L <: Shape, R <: Shape] extends Application2[R, L, R] {
  override val shape: R = r.shape
  override def l: ValueExpr[L]
  override def r: ValueExpr[R]
}

/**
  * specialized Application2, its left, right and output Expr are the same type of shape.
  * @tparam S type of shape for left, right and output Expr
  */
trait SameShapedApplication2[S <: Shape] extends Application2[S, S, S] {
  override val shape: S = l.shape
  override def l: ValueExpr[S]
  override def r: ValueExpr[S]
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
  * @tparam O type of shape for output Expr
  * @tparam S type of shape for argument Expr
  */
trait Application1[O <: Shape, S <: Shape] extends ValueExpr[O] {
  val shape: O
  def v: ValueExpr[S]
}

/**
  * specialized Application1, its input and output Expr are the same type of shape.
  * @tparam S type of shape for left, right and output Expr
  */
trait SameShapedApplication1[S <: Shape] extends Application1[S, S] {
  override val shape: S = v.shape

  override def v: ValueExpr[S]
}


// Unary Application

case class Apply0(v: V0, op: Op0) extends SameShapedApplication1[S0]

case class Apply00(l: V0, r: V0, op: Op00) extends SameShapedApplication2[S0]


case class Elementwise1(v: V1, op: Op0) extends SameShapedApplication1[S1]

case class Elementwise2(v: V2, op: Op0) extends SameShapedApplication1[S2]


case class Broadcast1(v: V1, op: Op0) extends SameShapedApplication1[S1]

case class Broadcast2(v: V2, op: Op0) extends SameShapedApplication1[S2]


case class Fold1(v: V1, op: Op00) extends Application1[S0, S1] { val shape: S0 = Shape0() }

case class Fold2(v: V2, op: Op00) extends Application1[S0, S2] { val shape: S0 = Shape0() }


case class FoldRowwise2(v: V2, op: Op00) extends Application1[S1, S2] { val shape: S1 = Shape1(v.shape._1) }

case class FoldColumnwise2(v: V2, op: Op00) extends Application1[S1, S2] { val shape: S1 = Shape1(v.shape._2) }


// Experimental Unary Application

case class VecFill(v: V0, shape: S1) extends Application1[S1, S0]

case class MatFill(v: V0, shape: S2) extends Application1[S2, S0]


case class MatFillAcrossRow(v: V1, numColumns: Int) extends Application1[S2, S1] { val shape: S2 = Shape2(v.shape._1, numColumns) }

case class MatFillAcrossColumn(v: V1, numRows: Int) extends Application1[S2, S1] { val shape: S2 = Shape2(numRows, v.shape._1) }


// Binary Application

case class Elementwise11(l: V1, r: V1, op: Op00) extends SameShapedApplication2[S1]

case class Elementwise22(l: V2, r: V2, op: Op00) extends SameShapedApplication2[S2]


case class Broadcast01(l: V0, r: V1, op: Op00) extends RightShapedApplication2[S0, S1]

case class Broadcast02(l: V0, r: V2, op: Op00) extends RightShapedApplication2[S0, S2]

case class Broadcast10(l: V1, r: V0, op: Op00) extends LeftShapedApplication2[S1, S0]

case class Broadcast20(l: V2, r: V0, op: Op00) extends LeftShapedApplication2[S2, S0]


case class Rowwise12(l: V1, r: V2, op: Op00) extends RightShapedApplication2[S1, S2]

case class Rowwise21(l: V2, r: V1, op: Op00) extends LeftShapedApplication2[S2, S1]


case class Columnwise12(l: V1, r: V2, op: Op00) extends RightShapedApplication2[S1, S2]

case class Columnwise21(l: V2, r: V1, op: Op00) extends RightShapedApplication2[S2, S1]

