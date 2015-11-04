package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{Constraint, Shape}

/**
  *  We have an another idea of forward/reverse that have more understandable/non-duplicative structure.
  * It is decomposing Application into Application and operator, like:
  *
  * case class Elementwise1(v: V, op: Operator) extends Application1 {
  *
  *   def forward(wrt: V): V = _forwardChain(wrt)
  *
  *   def _forwardChain(wrt: V): V = v.forward(wrt) :* op.deriv(v)
  *
  *   def reverse(adj: V): Grad = _reverseChain(wrt)
  *
  *   def _reverseChain(adj: V): Grad = v.reverse(adj :* op.deriv(v))
  *
  * }
  *
  * abstract class Operator {
  *
  *   def deriv(v: V): V
  *
  * }
  *
  * case object Pos extends Operator {
  *
  *   def deriv(v: V): V = Elementwise1(One(v.shape), Pos)
  *
  * }
  *
  *   But above implementation sometimes tends to produce more large/complex graph because of
  *  trivial nodes like One(). If we achieve making a good implementation of graph optimizer,
  *  it may be nice to adopt above impl.
  **/


abstract class Application1 extends V {

  def shape: Shape

  def v: V

}


abstract class Apply1(v: V) extends Application1 {

  final def shape: Shape = v.shape

}


abstract class Elementwise1(v: V) extends Application1 {

  final def shape: Shape = v.shape

}


@throws[Exception]
abstract class AxisWiseFold1(v: V, axis: Int) extends Application1 {

  Constraint.foldable(v)

  Constraint.indicatesValidAxis(v.shape, axis)

  def shape: Shape = v.shape.removeAxes(Seq(axis))

}


@throws[Exception]
abstract class Fill1(v: V, numNewAxes: Int) extends Application1 {

  Constraint.expandable(numNewAxes)

  def shape: Shape = v.shape.prependAxes(numNewAxes)

}
