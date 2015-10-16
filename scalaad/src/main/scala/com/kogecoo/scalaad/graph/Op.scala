package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.{Value, ValueRule}

import scala.language.higherKinds


case class Add[U[_], T](lhs: Node[U, T], rhs: Node[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp[U, T] {
  override def toString: String = s"(${ lhs.toString } + ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() + rhs()
  override def deriv(wrt: Node[U, T]): Value[U, T] = lhs.deriv(wrt) + rhs.deriv(wrt)
  override def propagate(g: Value[U, T]): Value[U, T] = lhs.propagate(g) + rhs.propagate(g)
}

case class Sub[U[_], T](lhs: Node[U, T], rhs: Node[U, T])(implicit r: ValueRule[U, T]) extends BinaryOp[U, T] {
  override def toString: String = s"(${ lhs.toString } - ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() - rhs()
  override def deriv(wrt: Node[U, T]): Value[U, T] = lhs.deriv(wrt) - rhs.deriv(wrt)
  override def propagate(g: Value[U, T]): Value[U, T] = lhs.propagate(g) + rhs.propagate(-g)
}

case class Mul[U[_], T](lhs: Node[U, T], rhs: Node[U, T])(implicit r: ValueRule[U, T]) extends BinaryOp[U, T] {
  override def toString: String = s"(${ lhs.toString } * ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() * rhs()
  override def deriv(wrt: Node[U, T]): Value[U, T] = {
    lhs.deriv(wrt) * rhs() + lhs() * rhs.deriv(wrt)
  }

  override def propagate(g: Value[U, T]): Value[U, T] = {
    lhs.propagate(g * rhs()) + rhs.propagate(g * lhs())
  }
}

case class Div[U[_], T](lhs: Node[U, T], rhs: Node[U, T])(implicit r: ValueRule[U, T]) extends BinaryOp[U, T] {
  override def toString: String = s"(${ lhs.toString } / ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() / rhs()
  override def deriv(wrt: Node[U, T]): Value[U, T] = {
    val rhs_val: Value[U, T] = rhs()
    val num: Value[U, T] = lhs.deriv(wrt) * rhs_val - lhs() * rhs.deriv(wrt)
    val den: Value[U, T] = rhs_val * rhs_val
    num / den
  }

  override def propagate(g: Value[U, T]): Value[U, T] = {
    val rhs_val = rhs()
    lhs.propagate(g / rhs_val) + rhs.propagate(-g * lhs() / rhs_val / rhs_val)
  }
}

case class Pos[U[_], T](v: Node[U, T])(implicit r: ValueRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"+(${ v })"
  override def apply(): Value[U, T] = +v()
  override def deriv(wrt: Node[U, T]): Value[U, T] = +v.deriv(wrt)
  override def propagate(g: Value[U, T]): Value[U, T] = v.propagate(g)
}

case class Neg[U[_], T](v: Node[U, T])(implicit r: ValueRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"-(${ v })"
  override def apply(): Value[U, T] = -v()
  override def deriv(wrt: Node[U, T]): Value[U, T] = -v.deriv(wrt)
  override def propagate(g: Value[U, T]): Value[U, T] = v.propagate(-g)
}

case class Transpose[U[_], T](v: Node[U, T])(implicit r: ValueRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"${ v }.T"
  override def apply(): Value[U, T] = v().T
  override def deriv(wrt: Node[U, T]): Value[U, T] = v.deriv(wrt).T
  override def propagate(g: Value[U, T]): Value[U, T] = v.propagate(g.T)
}
