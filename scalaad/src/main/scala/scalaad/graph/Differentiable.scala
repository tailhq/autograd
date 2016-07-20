package scalaad.graph

import scalaad.Shape


trait Differentiable { self: Expr =>


  def forward(wrt: DExpr): DExpr

  def reverse(adj: DExpr): Grad

  // Reference: https://en.wikipedia.org/wiki/Matrix_calculus#Vector-by-vector
  final def forwardOutputShape(wrt: Expr): Shape = wrt.shape.concat(self.shape)

}

/**
  *  We have an another idea of forward/reverse that have more understandable/non-duplicative structure.
  * It is decomposing Application into Application and operator, like:
  *
  *  * for Unary Applications:
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
  *
  *  * and for Binary Applications:
  *
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
  *   But above implementation sometimes tends to produce more large/complex graph because of
  *  trivial nodes like One(). If we achieve making a good implementation of graph optimizer,
  *  it may be nice to adopt above impl.
  **/
