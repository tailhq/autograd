package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.op.{Op0, Op00}
import com.kogecoo.scalaad.{Shape, Shape0, Shape1, Shape2}

import scala.language.higherKinds

/**
  * represents applying binary operation, which takes 2 Nodes as arguments.
 *
  * @tparam S a shape of this Node
  * @tparam L a shape of left Node
  * @tparam R a shape of right Node
  */
trait Application2[E[_] <: Expr[S], S <: Shape, L <: Shape, R <: Shape] extends E[S] {
  val shape: S
  def l: E[L]
  def r: E[R]
}

/**
  * specialized BinaryNode, its left and output Node are the same type of shape.
 *
  * @tparam L a shape of left and output Node
  * @tparam R a shape of right Node
  */
trait LeftShapedApplication2[E[_] <: Expr[L], L <: Shape, R <: Shape] extends Application2[E, L, L, R] {
  override val shape: L = l.shape
  override def l: E[L]
  override def r: E[R]
}

/**
  * specialized BinaryNode, its right and output Node are the same type of shape.
 *
  * @tparam L a shape of left Node
  * @tparam R a shape of right and output Node
  */
trait RightShapedApplication2[E[_] <: Expr[R], L <: Shape, R <: Shape] extends Application2[E, R, L, R] {
  override val shape: R = r.shape
  override def l: E[L]
  override def r: E[R]
}

/**
  * specialized BinaryNode, its left, right and output Node are the same type of shape.
 *
  * @tparam S type of shape for left, right and output Node
  */
trait SameShapedApplication2[E[_] <: Expr[S], S <: Shape] extends Application2[E, S, S, S] {
  override val shape: S = l.shape
  override def l: E[S]
  override def r: E[S]
}

/**
  * represents applying unary operation, which takes 1 Node as argument.
 *
  * @tparam O type of shape for output Node
  * @tparam S type of shape for argument Node
  */
trait Application1[E[_] <: Expr[S], O <: Shape, S <: Shape] extends E[O] {
  val shape: O
  def v: E[O]
}

/**
  * specialized UnaryNode, its input and output Node are the same type of shape.
 *
  * @tparam S type of shape for left, right and output Node
  */
trait SameShapedApplication1[E[_] <: Expr[S], S <: Shape] extends Application1[E, S, S] {
  override val shape: S = v.shape

  override def v: E[S]
}


// These nodes express higher-order functions

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

