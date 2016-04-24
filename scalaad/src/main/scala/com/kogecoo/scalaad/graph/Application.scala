package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.op.{Op0, Op00}
import com.kogecoo.scalaad.{Shape, Shape0, Shape1, Shape2}

import scala.language.higherKinds

/**
  * represents applying binary operation, which takes 2 Exprs arguments.
  *
  * @tparam E type of this Expr
  * @tparam S a shape of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right Expr
  */
trait Application2[E[_] <: Expr[S], S <: Shape, L <: Shape, R <: Shape] extends E[S] {
  val shape: S
  def l: E[L]
  def r: E[R]
}

/**
  * specialized Application2, its left and output Expr are the same type of shape.
  *
  * @tparam E type of this Expr
  * @tparam L a shape of left and output Expr
  * @tparam R a shape of right Expr
  */
trait LeftShapedApplication2[E[_] <: Expr[L], L <: Shape, R <: Shape] extends Application2[E, L, L, R] {
  override val shape: L = l.shape
  override def l: E[L]
  override def r: E[R]
}

/**
  * specialized Application2, its right and output Expr are the same type of shape.
  *
  * @tparam E type of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right and output Expr
  */
trait RightShapedApplication2[E[_] <: Expr[R], L <: Shape, R <: Shape] extends Application2[E, R, L, R] {
  override val shape: R = r.shape
  override def l: E[L]
  override def r: E[R]
}

/**
  * specialized Application2, its left, right and output Expr are the same type of shape.
  *
  * @tparam E type of this Expr
  * @tparam S type of shape for left, right and output Expr
  */
trait SameShapedApplication2[E[_] <: Expr[S], S <: Shape] extends Application2[E, S, S, S] {
  override val shape: S = l.shape
  override def l: E[S]
  override def r: E[S]
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
  *
  * @tparam E type of this Expr
  * @tparam O type of shape for output Expr
  * @tparam S type of shape for argument Expr
  */
trait Application1[E[_] <: Expr[S], O <: Shape, S <: Shape] extends E[O] {
  val shape: O
  def v: E[O]
}

/**
  * specialized Application1, its input and output Expr are the same type of shape.
  *
  * @tparam E type of this Expr
  * @tparam S type of shape for left, right and output Expr
  */
trait SameShapedApplication1[E[_] <: Expr[S], S <: Shape] extends Application1[E, S, S] {
  override val shape: S = v.shape

  override def v: E[S]
}


// Unary Application

case class Apply0[E[_] <: Expr[_]]
  (v: E[S0], op: Op0) extends SameShapedApplication1[E, S0]

case class Apply00[E[_] <: Expr[_]]
  (l: E[S0], r: E[S0], op: Op00) extends SameShapedApplication2[E, S0]


case class Elementwise1[E[_] <: Expr[_]]
  (v: E[S1], op: Op0) extends SameShapedApplication1[E, S1]

case class Elementwise2[E[_] <: Expr[_]]
  (v: E[S2], op: Op0) extends SameShapedApplication1[E, S2]


case class Broadcast1[E[_] <: Expr[_]]
  (v: E[S1], op: Op0) extends SameShapedApplication1[E, S1]

case class Broadcast2[E[_] <: Expr[_]]
  (v: E[S2], op: Op0) extends SameShapedApplication1[E, S2]


case class Fold1[E[_] <: Expr[_]]
  (v: E[S1], op: Op00) extends Application1[E, S0, S1] { val shape: S0 = Shape0() }

case class Fold2[E[_] <: Expr[_]]
  (v: E[S2], op: Op00) extends Application1[E, S0, S2] { val shape: S0 = Shape0() }


case class FoldRowwise2[E[_] <: Expr[_]]
  (v: E[S2], op: Op00) extends Application1[E, S1, S2] { val shape: S1 = Shape1(v.shape._1) }

case class FoldColumnwise2[E[_] <: Expr[_]]
  (v: E[S2], op: Op00) extends Application1[E, S1, S2] { val shape: S1 = Shape1(v.shape._2) }


// Experimental Unary Application

case class VecFill[E[_] <: Expr[_]]
  (v: E[S0], shape: S1) extends Application1[E, S1, S0]

case class MatFill[E[_] <: Expr[_]]
  (v: E[S0], shape: S2) extends Application1[E, S2, S0]


case class MatFillAcrossRow[E[_] <: Expr[_]]
  (v: E[S1], numColumns: Int) extends Application1[E, S2, S1] { val shape: S2 = Shape2(v.shape._1, numColumns) }

case class MatFillAcrossColumn[E[_] <: Expr[_]]
  (v: E[S1], numRows: Int) extends Application1[E, S2, S1] { val shape: S2 = Shape2(numRows, v.shape._1) }


// Binary Application

case class Elementwise11[E[_] <: Expr[_]]
  (l: E[S1], r: E[S1], op: Op00) extends SameShapedApplication2[E, S1]

case class Elementwise22[E[_] <: Expr[_]]
  (l: E[S2], r: E[S2], op: Op00) extends SameShapedApplication2[E, S2]


case class Broadcast01[E[_] <: Expr[_]]
  (l: E[S0], r: E[S1], op: Op00) extends RightShapedApplication2[E, S0, S1]

case class Broadcast02[E[_] <: Expr[_]]
  (l: E[S0], r: E[S2], op: Op00) extends RightShapedApplication2[E, S0, S2]

case class Broadcast10[E[_] <: Expr[_]]
  (l: E[S1], r: E[S0], op: Op00) extends LeftShapedApplication2[E, S1, S0]

case class Broadcast20[E[_] <: Expr[_]]
  (l: E[S2], r: E[S0], op: Op00) extends LeftShapedApplication2[E, S2, S0]


case class Rowwise12[E[_] <: Expr[_]]
  (l: E[S1], r: E[S2], op: Op00) extends RightShapedApplication2[E, S1, S2]

case class Rowwise21[E[_] <: Expr[_]]
  (l: E[S2], r: E[S1], op: Op00) extends LeftShapedApplication2[E, S2, S1]


case class Columnwise12[E[_] <: Expr[_]]
  (l: E[S1], r: E[S2], op: Op00) extends RightShapedApplication2[E, S1, S2]

case class Columnwise21[E[_] <: Expr[_]]
  (l: E[S2], r: E[S1], op: Op00) extends RightShapedApplication2[E, S2, S1]


// TODO
case object Transpose1

case object Transpose2

