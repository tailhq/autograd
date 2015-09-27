package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.rule.Implicits._

import scala.language.higherKinds


case class Add[U[_], T](lhs: Node[U, T], rhs: Node[U, T])(implicit r: ValueRule[U, T]) extends BinaryOp[U, T] {
  override def toString: String = s"(${ lhs.toString } + ${ rhs.toString })"
  override def apply(): U[T] = (lhs, rhs) match {
    case (lhs: ScalarConst[U, T], rhs: ScalarConst[U, T]) => r.wrap(lhs.unwrap() +:+ rhs.unwrap()) // FIXME: optimize
    case (lhs,                    rhs: ScalarConst[U, T]) => lhs() +> rhs.unwrap()
    case (lhs: ScalarConst[U, T], rhs                   ) => lhs.unwrap() +< rhs()
    case _                                                => lhs() + rhs()
  }

  override def deriv(wrt: Node[U, T]): U[T] = {
    lhs.deriv(wrt) + rhs.deriv(wrt)
  }

  override def propagate(g: U[T]): U[T] = lhs.propagate(g) + rhs.propagate(g)
}

case class Sub[U[_], T](lhs: Node[U, T], rhs: Node[U, T])(implicit r: ValueRule[U, T]) extends BinaryOp[U, T] {
  override def toString: String = s"(${ lhs.toString } - ${ rhs.toString })"
  override def apply(): U[T] = (lhs, rhs) match {
    case (lhs: ScalarConst[U, T], rhs: ScalarConst[U, T]) => r.wrap(lhs.unwrap() -:- rhs.unwrap()) // FIXME: optimize
    case (lhs,                    rhs: ScalarConst[U, T]) => lhs() -> rhs.unwrap()
    case (lhs: ScalarConst[U, T], rhs                   ) => lhs.unwrap() -< rhs()
    case _                                                => lhs() - rhs()
  }

  override def deriv(wrt: Node[U, T]): U[T] = {
    lhs.deriv(wrt) - rhs.deriv(wrt)
  }

  override def propagate(g: U[T]): U[T] = lhs.propagate(g) + rhs.propagate(-g)
}

case class Mul[U[_], T](lhs: Node[U, T], rhs: Node[U, T])(implicit r: ValueRule[U, T]) extends BinaryOp[U, T] {
  override def toString: String = s"(${ lhs.toString } * ${ rhs.toString })"
  override def apply(): U[T] = (lhs, rhs) match {
    case (lhs: ScalarConst[U, T], rhs: ScalarConst[U, T]) => r.wrap(lhs.unwrap() *:* rhs.unwrap()) // FIXME: optimize
    case (lhs,                    rhs: ScalarConst[U, T]) => lhs() *> rhs.unwrap()
    case (lhs: ScalarConst[U, T], rhs                   ) => lhs.unwrap() *< rhs()
    case _                                                => lhs() * rhs()
  }

  override def deriv(wrt: Node[U, T]): U[T] = {
    lhs.deriv(wrt) * rhs() + lhs() * rhs.deriv(wrt)
  }

  override def propagate(g: U[T]): U[T] = lhs.propagate(g * rhs()) + rhs.propagate(g * lhs())
}

case class Div[U[_], T](lhs: Node[U, T], rhs: Node[U, T])(implicit r: ValueRule[U, T]) extends BinaryOp[U, T] {
  override def toString: String = s"(${ lhs.toString } / ${ rhs.toString })"
  override def apply(): U[T] = (lhs, rhs) match {
    case (lhs: ScalarConst[U, T], rhs: ScalarConst[U, T]) => r.wrap(lhs.unwrap() /:/ rhs.unwrap()) // FIXME: optimize
    case (lhs,                    rhs: ScalarConst[U, T]) => lhs() /> rhs.unwrap()
    case (lhs: ScalarConst[U, T], rhs                   ) => lhs.unwrap() /< rhs()
    case _                                                => lhs() / rhs()
  }

  override def deriv(wrt: Node[U, T]): U[T] = {
    val rhs_val = rhs()
    val num = lhs.deriv(wrt) * rhs_val - lhs() * rhs.deriv(wrt)
    val den = rhs_val * rhs_val
    num / den
  }

  override def propagate(g: U[T]): U[T] = {
    val rhs_val = rhs()
    lhs.propagate(g / rhs_val) + rhs.propagate(-g * lhs() / rhs_val)
  }
}

case class Pos[U[_], T](v: Node[U, T])(implicit r: ValueRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"+(${ v })"
  override def apply(): U[T] = +v()
  override def deriv(wrt: Node[U, T]): U[T] = +v.deriv(wrt)
  override def propagate(g: U[T]): U[T] = v.propagate(g)
}

case class Neg[U[_], T](v: Node[U, T])(implicit r: ValueRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"-(${ v })"
  override def apply(): U[T] = -v()
  override def deriv(wrt: Node[U, T]): U[T] = -v.deriv(wrt)
  override def propagate(g: U[T]): U[T] = v.propagate(-g)
}
