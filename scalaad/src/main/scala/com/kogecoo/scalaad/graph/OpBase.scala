package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape

/**
  * represents Binary operator or functions which take 2 arguments
  * @tparam S type of shape for output Node
  * @tparam L type of shape for left and output Node
  * @tparam R type of shape for right Node
  */
trait BinaryOp[S <: Shape, L <: Shape, R <: Shape] extends Node[S] {
  val shape: S
  def l: Node[L]
  def r: Node[R]
}

/**
  * specific BinaryOp its output and left Node have the same order and shape
  * @tparam L type of shape for left and output Node
  * @tparam R type of shape for right Node
  */
trait LeftShapedBinaryOp[L <: Shape, R <: Shape] extends BinaryOp[L, L, R] {
  override val shape: L = l.shape
  override def l: Node[L]
  override def r: Node[R]
}

/**
  * specific BinaryOp its output and right Node have the same order and shape
  * @tparam L type of shape for left Node
  * @tparam R type of shape for right and output Node
  */
trait RightShapedBinaryOp[L <: Shape, R <: Shape] extends BinaryOp[R, L, R] {
  override val shape: R = r.shape
  override def l: Node[L]
  override def r: Node[R]
}

/**
  * specific BinaryOp its left, right and output Node have the same order and shape
  * @tparam S type of shape for left, right and output Node
  */
trait ShapeEquivBinaryOp[S <: Shape] extends BinaryOp[S, S, S] {
  override val shape: S = l.shape
  override def l: Node[S]
  override def r: Node[S]
}

/**
  * represents Unary operator or functions which take 1 argument
  * @tparam O type of shape for output Node
  * @tparam S type of shape for argument Node
  */
trait UnaryOp[O <: Shape, S <: Shape] extends Node[O] {
  val shape: O
  def v: Node[S]
}

/**
  * specific UnaryOp its input and output Node have the same order and shape.
  * @tparam S type of shape for left, right and output Node
  */
trait ShapeEquivUnaryOp[S <: Shape] extends UnaryOp[S, S] {
  override val shape: S = v.shape
  override def v: Node[S]
}


