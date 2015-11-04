package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph.Expr


object Constraint {

  @throws[Exception]
  def satisfy(cond: Boolean, msg: String): Unit = {
    if (!cond) throw new Exception(msg)
  }

  @throws[Exception]
  def commonShape(l: Expr, r: Expr): Unit = satisfy(
    l.shape.order == r.shape.order && l.shape == r.shape,
    s"Shapes (${l.shape}) and (${r.shape}) must be equivalent."
  )

  @throws[Exception]
  def leftOrderBiggerThanRight(l: Expr, r: Expr): Unit = satisfy(
    l.shape.order > r.shape.order,
    s"The order of the left (${l.shape}) must be bigger than the right's (${r.shape})."
  )

  @throws[Exception]
  def rightOrderBiggerThanLeft(l: Expr, r: Expr): Unit = satisfy(
    l.shape.order < r.shape.order,
    s"The order of the left (${l.shape}) must be smaller than the right's (${r.shape})."
  )

  @throws[Exception]
  def broadcastableToLeft(l: Expr, r: Expr): Unit = satisfy(
    l.shape.underlying.take(r.shape.order) == r.shape.underlying,
    {
      val hint = l.shape.removeAxes(l.shape.underlying.indices.toList.drop(r.shape.order))
      s"Broadcast cannot perform for shape pair (${l.shape}) and (${r.shape}). Maybe the left shape must be $hint."
    }
  )

  @throws[Exception]
  def broadcastableToRight(l: Expr, r: Expr): Unit = satisfy(
    r.shape.underlying.take(l.shape.order) == l.shape.underlying,
    {
      val hint = r.shape.removeAxes(r.shape.underlying.indices.toList.drop(l.shape.order))
      s"Broadcast cannot perform for shape pair (${l.shape}) and (${r.shape}). Maybe the right shape must be $hint."
    }
  )

  @throws[Exception]
  def indicatesValidAxis(s: Shape, axis: Int): Unit = satisfy(
    axis >= 0 && axis < s.order,
    s"Axis $axis exceeds the dimension of $s"
  )

  @throws[Exception]
  def indicatesValidAxes(s: Shape, axes: Seq[Int]): Unit = {
    axes.foreach { axis => indicatesValidAxis(s, axis) }
  }

  @throws[Exception]
  def foldable(e: Expr): Unit = satisfy(
    e.shape.order > 0,
    s"Fold cannot perform for the order of Expr $e. It must be > 0"
  )

  @throws[Exception]
  def expandable(numNewAxes: Int): Unit = satisfy(
    numNewAxes >= 0,
    s"The size of numNewAxes for Expand Expr must be >= 0"
  )

  @throws[Exception]
  def allAxesHaveCommonLength(shape: Shape): Unit = satisfy(
    shape.order < 2 || shape.underlying.tail.forall(_ == shape.underlying.head),
    s"All axes needs to be the same $shape"
  )

}
