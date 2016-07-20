package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shorthands.const._
import com.kogecoo.scalaad.Shorthands.math._
import com.kogecoo.scalaad.Shorthands.syntax._
import com.kogecoo.scalaad.{Constraint, Shape}


// Expr -> Expr

case class Sin(override val v: DExpr) extends Elementwise1 with Differentiable {

  final def forward(wrt: DExpr): DExpr = cos(v.forward(wrt))

  final def reverse(adj: DExpr): Grad = v.reverse(cos(adj))

}


case class Cos(override val v: DExpr) extends Elementwise1 with Differentiable {

  final def forward(wrt: DExpr): DExpr = -sin(v.forward(wrt))

  final def reverse(adj: DExpr): Grad = v.reverse(-sin(adj))
}


case class Tan(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = {
    val t = tan(v.forward(wrt))
    one(v) :- (t :* t)
  }

  def reverse(adj: DExpr): Grad = v.reverse(one(adj) :- (tan(adj) :* tan(adj)))

}


case class Asin(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = {
    val a = v.forward(wrt)
    one(a) :/ sqrt(one(a) :- (a :* a))
  }

  def reverse(adj: DExpr): Grad = v.reverse(one(adj) :/ sqrt(one(adj) :- (adj :* adj)))

}


case class Acos(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = {
    val a = v.forward(wrt)
    -(one(a) :/ sqrt(one(a) :- (a :* a)))
  }

  def reverse(adj: DExpr): Grad = v.reverse(-(one(adj) :/ sqrt(one(adj) :- (adj :* adj))))

}


case class Atan(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = {
    val a = v.forward(wrt)
    one(a) :/ (one(a) :+ (a :* a))
  }

  def reverse(adj: DExpr): Grad = v.reverse(one(adj) :/ (one(adj) :+ (adj :* adj)))

}


case class Sinh(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = cosh(v.forward(wrt))

  def reverse(adj: DExpr): Grad = v.reverse(cosh(adj))

}


case class Cosh(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = sinh(v.forward(wrt))

  def reverse(adj: DExpr): Grad = v.reverse(sinh(adj))

}


case class Tanh(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = {
    val a = v.forward(wrt)
    one(a) :- (tan(a) :* tan(a))
  }

  def reverse(adj: DExpr): Grad = v.reverse(one(adj) :- (tan(adj) :* tan(adj)))

}


case class Ln(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = {
    val a = v.forward(wrt)
    one(a) :/ a
  }

  def reverse(adj: DExpr): Grad = v.reverse(one(adj) :/ adj)

}


case class Exp(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = exp(v.forward(wrt))

  def reverse(adj: DExpr): Grad = v.reverse(exp(adj))

}


case class Sqrt(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = half(v) :/ sqrt(v)

  def reverse(adj: DExpr): Grad = v.reverse(half(adj) :/ sqrt(adj))

}


case class Abs(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = where(v :>= zero(v), one(v), -one(v))

  def reverse(adj: DExpr): Grad = v.reverse(where(v :>= zero(v), adj, -adj))

}


// Expr -> Expr with reducing shape order

/*
case object L2Norm extends UnaryFoldOp {

  def deriv(v: V): V = two(v) * v

}
*/

case class Sum1(override val v: DExpr, override val axis: Int) extends AxisWiseFold1 with Differentiable {

  def forward(wrt: DExpr): DExpr = sum(v.forward(wrt), axis)

  def reverse(adj: DExpr): Grad = v.reverse(sum(adj, axis))

}

case class Max1(override val v: DExpr, override val axis: Int) extends AxisWiseFold1 with Differentiable {

  def forward(wrt: DExpr): DExpr = max(v.forward(wrt), axis)

  def reverse(adj: DExpr): Grad = v.reverse(max(adj, axis))

}

case class Min1(override val v: DExpr, override val axis: Int) extends AxisWiseFold1 with Differentiable {

  def forward(wrt: DExpr): DExpr = min(v.forward(wrt), axis)

  def reverse(adj: DExpr): Grad = v.reverse(min(adj, axis))

}


// (Expr[Shape], Expr[Shape]) -> Expr[Shape0]

case class Pow(override val l: DExpr, override val r: DExpr) extends Elementwise2 with Differentiable {

  def forward(wrt: DExpr): DExpr = {
    val (dl, dr) = deriv(l, r)
    (l.forward(wrt) :* dr) :+ (dl :* r.forward(wrt))
  }

  def reverse(adj: DExpr): Grad = {
    val (dl, dr) = deriv(l, r)
    l.reverse(adj :* dr) ++ r.reverse(dl :* adj)
  }

  private[this] def deriv(l: DExpr, r: DExpr): (DExpr, DExpr) = {
    val dl = ln(l) :* pow(l, r)
    val dr = r :* pow(l, r :- one(r))
    (dl, dr)
  }

}

case class Max2(override val l: DExpr, override val r: DExpr) extends Elementwise2 with Differentiable {

  def forward(wrt: DExpr): DExpr = where(l :>= r, l.forward(wrt), r.forward(wrt))

  def reverse(adj: DExpr): Grad = {
    val a = l.reverse(where(l :>= r, adj, zero(r)))
    val b = r.reverse(where(l :>= r, zero(l), adj))
    a ++ b
  }

}

case class Min2(override val l: DExpr, override val r: DExpr) extends Elementwise2 with Differentiable {

  def forward(wrt: DExpr): DExpr = where(l :<= r, l.forward(wrt), r.forward(wrt))

  def reverse(adj: DExpr): Grad = {
    val a = l.reverse(where(l :<= r, adj, zero(r)))
    val b = r.reverse(where(l :<= r, zero(l), adj))
    a ++ b
  }

}


// (Expr[Shape1], Expr[Shape1]) -> Expr[Shape0]

// TODO: Same behavior with http://docs.scipy.org/doc/numpy/reference/generated/numpy.dot.html
case class Dot(override val l: DExpr, override val r: DExpr) extends Application2 with Differentiable {

  // FIXME:
  def shape: Shape = l.shape.removeAxes(Seq(l.shape.order - 1)) // drop last axis

  def forward(wrt: DExpr): DExpr = dot(l.forward(wrt), r) :+ dot(l, r.forward(wrt))

  def reverse(adj: DExpr): Grad = l.reverse(dot(adj, r)) ++ r.reverse(dot(l, adj))

}


// (Expr[Shape2], Expr[Shape2]) -> Expr[Shape2]

@throws[Exception]
case class MatMul(override val l: DExpr, override val r: DExpr) extends Application2 with Differentiable {

  Constraint.satisfy(l.shape.order >= 2, s"The order of lhs shape for MatMul needs to be >= 2")

  Constraint.satisfy(r.shape.order >= 2, s"The order of rhs shape for MatMul needs to be >= 2")

  Constraint.satisfy(l.shape.at(-1) == r.shape.at(-2), s"Shapes ${l.shape} and ${r.shape} are not aligned for MatMul")

  Constraint.satisfy(l.shape.slice(0, -2) == r.shape.slice(0, -2), s"Shapes ${l.shape} and ${r.shape} are not aligned for MatMul")


  def shape: Shape = {
    val s = Shape(l.shape.at(-2), r.shape.at(-1))
    if (l.shape.order > 2 && r.shape.order > 2) {
      l.shape.slice(0, -2).concat(s)
    } else s
  }

  def forward(wrt: DExpr): DExpr = matmul(l.forward(wrt), r) :+ matmul(l, r.forward(wrt))

  def reverse(adj: DExpr): Grad = l.reverse(matmul(adj, r)) ++ r.reverse(matmul(l, adj))

}
