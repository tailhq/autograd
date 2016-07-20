package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.graph._


object StdElementwiseOp {

  def nullary(n: Expr): () => T0 = n match {
    case Zero(_) => () => 0.0
    case Half(_) => () => 0.5
    case One (_) => () => 1.0
  }

  def unary(n: Expr): (T0 => T0) = n match {
    case _: Pos      => +_
    case _: Neg      => -_
    case _: Identity => v => v
    case _: Sign     => v => math.abs(v) / v

    case _: Sin => math.sin
    case _: Cos => math.cos
    case _: Tan => math.tan

    case _: Asin => math.asin
    case _: Acos => math.acos
    case _: Atan => math.atan

    case _: Sinh => math.sinh
    case _: Cosh => math.cosh
    case _: Tanh => math.tanh

    case _: Ln   => math.log
    case _: Exp  => math.exp
    case _: Sqrt => math.sqrt

    case _: Abs  => math.abs
  }

  def binary(n: Expr): (T0, T0) => T0 = n match {
    case _: Add => (x, y) => x + y
    case _: Sub => (x, y) => x - y
    case _: Mul => (x, y) => x * y
    case _: Div => (x, y) => x / y

    case _: Pow  => (x, y) => math.pow(x, y)
    case _: Max2 => (x, y) => math.max(x, y)
    case _: Min2 => (x, y) => math.min(x, y)
  }

}


object StdElementwiseOpB {

  def unary(n: Expr): (B0 => B0) = n match {
    case _: Not => v => !v
  }

  def binary(n: Expr): ((B0, B0) => B0) = n match {
    case _: And => (x, y) => x && y
    case _: Or  => (x, y) => x || y
  }

}


object StdElementwiseOpC {

  def binary(n: Expr): ((T0, T0) => B0) = n match {
    case _: Eq  => (x, y) => x == y
    case _: Neq => (x, y) => x != y
    case _: Lt  => (x, y) => x < y
    case _: Lte => (x, y) => x <= y
    case _: Gt  => (x, y) => x > y
    case _: Gte => (x, y) => x >= y
  }

}
