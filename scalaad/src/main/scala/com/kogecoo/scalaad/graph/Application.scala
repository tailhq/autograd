package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{Constraint, Shape}



/**
  *  Nullary Applications
 */

trait Application0 extends Expr


trait Elementwise0 extends Application0


/**
  *  Unary Applications
 */

trait Application1 extends Expr {

  def shape: Shape = v.shape

  def v: Expr

}


trait Elementwise1 extends Application1


@throws[Exception]
trait AxisWiseFold1 extends Application1 {

  Constraint.foldable(v)

  Constraint.indicatesValidAxis(v.shape, axis)

  override def shape: Shape = v.shape.removeAxes(Seq(axis))

  val axis: Int

}

/**
  * currently not used
  *
  */
@throws[Exception]
trait Fill1 extends Application1 {

  Constraint.expandable(numNewAxes)

  override def shape: Shape = v.shape.prependAxes(numNewAxes)

  val numNewAxes: Int

}

/**
  *  Binary Applications
 */

trait Application2 extends Expr {

  def shape: Shape

  def l: Expr

  def r: Expr

}

trait Elementwise2 extends Application2 {

  @throws[Exception]
  final def shape: Shape = BroadcastHelper.computeOutputShape(Seq[Shape](l.shape, r.shape))

}



