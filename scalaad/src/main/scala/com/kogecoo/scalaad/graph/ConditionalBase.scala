package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape


/**
  * represents Binary condition which take 2 boolean arguments
  * @tparam S type of shape for output BoolNode
  * @tparam L type of shape for left and output BoolNode
  * @tparam R type of shape for right BoolNode
  */
trait BinaryBoolOp[S <: Shape, L <: Shape, R <: Shape] extends BoolNode[S] {
  val shape: S
  def l: BoolNode[L]
  def r: BoolNode[R]
}

/**
  * represents Binary condition which take 2 arguments
  * @tparam S type of shape for output BoolNode
  * @tparam L type of shape for left and output BoolNode
  * @tparam R type of shape for right Node
  */
trait BinaryCond[S <: Shape, L <: Shape, R <: Shape] extends BoolNode[S] {
  val shape: S
  def l: Node[L]
  def r: Node[R]
}

/**
  * specific BinaryOp its output BoolNode and left Node have the same order and shape
  * @tparam L type of shape for left and output BoolNode
  * @tparam R type of shape for right Node
  */
trait LeftShapedBinaryCond[L <: Shape, R <: Shape] extends BinaryCond[L, L, R] {
  override val shape: L = l.shape
  override def l: Node[L]
  override def r: Node[R]
}

/**
  * specific BinaryCond its output BoolNode and right Node have the same order and shape
  * @tparam L type of shape for left Node
  * @tparam R type of shape for right and output BoolNode
  */
trait RightShapedBinaryCond[L <: Shape, R <: Shape] extends BinaryCond[R, L, R] {
  override val shape: R = r.shape
  override def l: Node[L]
  override def r: Node[R]
}

/**
  * specific BinaryCond its left, right and output BoolNode have the same order and shape
  * @tparam S type of shape for left, right and output BoolNode
  */
trait ShapeEquivBinaryCond[S <: Shape] extends BinaryCond[S, S, S] {
  override val shape: S = l.shape
  override def l: Node[S]
  override def r: Node[S]
}

/**
  * represents Unary condition which take 1 argument
  * @tparam O type of shape for output BoolNode
  * @tparam S type of shape for argument Node
  */
trait UnaryCond[O <: Shape, S <: Shape] extends BoolNode[O] {
  val shape: O
  def v: BoolNode[S]
}

/**
  * specific UnaryCond its input and output BoolNode have the same order and shape.
  * @tparam S type of shape for left, right and output BoolNode
  */
trait ShapeEquivUnaryCond[S <: Shape] extends UnaryCond[S, S] {
  override val shape: S = v.shape
  override def v: BoolNode[S]
}

