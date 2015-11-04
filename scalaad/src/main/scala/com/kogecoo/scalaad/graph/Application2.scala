package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape

/**
  *  Similar with Application1, we have an another idea of forward/reverse implementation.
  *
  * case class Elementwise2(l: V, r: V, op: BinaryOperator) extends Application1 {
  *
  *   def forward(wrt: V): V = _forwardChain(wrt)
  *
  *   def _forwardChain(wrt: V): V = {
  *     val (dl, dr) = op.deriv(l, r)
  *     val fl = l.forward(wrt)
  *     val fr = r.forward(wrt)
  *     (fl :* dr) :+ (dl :* fr)
  *   }
  *
  *   def reverse(adj: V): Grad = _reverseChain(wrt)
  *
  *   def _reverseChain(adj: V): Grad = {
  *     val (dl, dr) = op.deriv(l, r)
  *     l.reverse(adj  :* dr) ++ r.reverse(dl :* adj)
  *   }
  *
  * }
  *
  * abstract class Operator {
  *
  *   def deriv(l: V, r: V): (V, V)
  *
  * }
  *
  * case object Div extends BinaryOperator {
  *
  *   def deriv(l: V, r: V): V = (l, r)
  *     override def deriv(l: V, r: V): (V, V) = {
  *     val first = one(l) :/ r
  *     val second = -l :/ (r :* r)
  *     (first, second)
  *   }
  *
  * }
  *
  **/

trait Application2 extends V {

  def shape: Shape

  def l: V

  def r: V

}

abstract class Elementwise2(l: V, r: V) extends Application2 {

  @throws[Exception]
  final def shape: Shape = BroadcastHelper.computeOutputShape(Seq[Shape](l.shape, r.shape))

}

