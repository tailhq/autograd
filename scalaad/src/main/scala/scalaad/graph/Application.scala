package scalaad.graph

import scalaad.{Constraint, Shape}



/**
  *  Nullary Applications
 */

trait Application0[D <: DType] extends Expr[D]


trait Elementwise0[D <: DType] extends Application0[D]


/**
  *  Unary Applications
 */

trait Application1[O <: DType, I <: DType] extends Expr[O] {

  def shape: Shape = v.shape

  def v: Expr[I]

}


trait Elementwise1[O <: DType, I <: DType] extends Application1[O, I]



@throws[Exception]
trait AxisWiseFold1[D <: DType] extends Application1[D, D] {

  Constraint.foldable(v)

  Constraint.indicatesValidAxis(v.shape, axis)

  override def shape: Shape = v.shape.removeAxes(Seq(axis))

  val axis: Int

}

@throws[Exception]
trait Fill1[D <: DType] extends Application1[D, D] {

  Constraint.expandable(numNewAxes)

  override def shape: Shape = v.shape.prependAxes(numNewAxes)

  val numNewAxes: Int

}

/**
  *  Binary Applications
 */

trait Application2[O <: DType, I <: DType] extends Expr[O] {

  def shape: Shape

  def l: Expr[I]

  def r: Expr[I]

}

trait Elementwise2[O <: DType, I <: DType] extends Application2[O, I] {

  @throws[Exception]
  final def shape: Shape = BroadcastHelper.computeOutputShape(Seq[Shape](l.shape, r.shape))

}

