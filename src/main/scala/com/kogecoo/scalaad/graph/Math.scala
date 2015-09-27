package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.MathRule
import com.kogecoo.scalaad.rule.Implicits._

import scala.language.higherKinds


case class sin[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"sin(${ v })"
  override def apply(): U[T] = r.sin(v())
  override def deriv(wrt: Node[U, T]): U[T] = r.cos(v()) * v.deriv(wrt)
  override def propagate(g: U[T]): U[T] = v.propagate(g * r.cos(v()))
}

case class cos[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"cos(${ v })"
  override def apply(): U[T] = r.cos(v())
  override def deriv(wrt: Node[U, T]): U[T] = -r.sin(v()) * v.deriv(wrt)
  override def propagate(g: U[T]): U[T] = v.propagate(g * -r.sin(v()))
}

case class tan[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"tan(${ v })"
  override def apply(): U[T] = r.tan(v())
  override def deriv(wrt: Node[U, T]): U[T] = {
    val v_val = v()
    v.deriv(wrt) * (r.zeroMul + r.tan(v_val) * r.tan(v_val))
  }

  override def propagate(g: U[T]): U[T] = {
    val v_val = v()
    v.propagate(g * (r.zeroMul + r.tan(v_val) * r.tan(v_val)))
  }
}

case class ln[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"ln(${ v })"
  override def apply(): U[T] = r.ln(v())
  override def deriv(wrt: Node[U, T]): U[T] = v.deriv(wrt) / v()
  override def propagate(g: U[T]): U[T] = v.propagate(g / v())
}


