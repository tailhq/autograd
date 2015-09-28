package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.MathRule
import com.kogecoo.scalaad.rule.Implicits._

import scala.language.higherKinds


case class sin[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"sin(${ v })"
  override def apply(): Value[U, T] = sin_#(v())
  override def deriv(wrt: Node[U, T]): Value[U, T] = cos_#(v()) * v.deriv(wrt)
  override def propagate(g: Value[U, T]): Value[U, T] = g * cos_#(v())
}

case class cos[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"cos(${ v })"
  override def apply(): Value[U, T] = cos_#(v())
  override def deriv(wrt: Node[U, T]): Value[U, T] = -sin_#(v()) * v.deriv(wrt)
  override def propagate(g: Value[U, T]): Value[U, T] = -sin_#(v()) * g
}

case class tan[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"tan(${ v })"
  override def apply(): Value[U, T] = tan_#(v())
  override def deriv(wrt: Node[U, T]): Value[U, T] = {
    val v_val: Value[U, T] = v()
    val tan2: Value[U, T] = tan_#(v_val) * tan_#(v_val)
    v.deriv(wrt) * (vr.zeroMul + tan2)
  }

  override def propagate(g: Value[U, T]): Value[U, T] = {
    val v_val: Value[U, T] = v()
    v.propagate(g * (vr.zeroMul + tan_#(v_val) * tan_#(v_val)))
  }
}

case class ln[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp[U, T] {
  override def toString: String = s"ln(${ v })"
  override def apply(): Value[U, T] = ln_#(v())
  override def deriv(wrt: Node[U, T]): Value[U, T] = v.deriv(wrt) / v()
  override def propagate(g: Value[U, T]): Value[U, T] = v.propagate(g / v())
}


