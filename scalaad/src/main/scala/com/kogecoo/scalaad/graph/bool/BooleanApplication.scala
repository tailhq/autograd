package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.{B, BroadcastHelper}


// TODO: code-sharing with Application

// Unary BooleanApplication

/**
  * represents applying unary operation, which takes 1 Expr as argument.
 *
  */
trait BooleanApplication1 extends B {

  def shape: Shape

  def v: B

}


abstract class Elementwise1B(v: B) extends BooleanApplication1 {

  final def shape: Shape = v.shape

}


/**
  * represents applying binary operation, which takes 2 Exprs arguments.
 *
  */
trait BooleanApplication2 extends B {

  def shape: Shape

  def l: B

  def r: B

}


abstract class Elementwise2B(l: B, r: B) extends BooleanApplication2 {

  @throws[Exception]
  final def shape: Shape = BroadcastHelper.computeOutputShape(Seq[Shape](l.shape, r.shape))

}
