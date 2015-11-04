package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.{B, BroadcastHelper, V}


// TODO: code-sharing with Application

/**
  * represents applying binary comparing operation, which takes 2 Exprs arguments.
  *
  */
trait ComparisonApplication2 extends B {

  def shape: Shape

  def l: V

  def r: V

}


abstract class Elementwise2C(l: V, r: V) extends ComparisonApplication2 {

  @throws[Exception]
  final def shape: Shape = BroadcastHelper.computeOutputShape(Seq[Shape](l.shape, r.shape))

}
