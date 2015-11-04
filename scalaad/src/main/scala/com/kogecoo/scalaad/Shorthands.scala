package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph._


object Shorthands {

  object const {

    def zero(v: V): V = Zero(v.shape)

    def half(v: V): V = Half(v.shape)

    def one(v: V): V = One(v.shape)

    def two(v: V): V = Two(v.shape)

  }

  object math {

    // unary

    def sign(v: V): V = Sign(v)

    def sin(v: V): V = Sin(v)

    def cos(v: V): V = Cos(v)

    def tan(v: V): V = Tan(v)

    def asin(v: V): V = Asin(v)

    def acos(v: V): V = Acos(v)

    def atan(v: V): V = Atan(v)

    def sinh(v: V): V = Sinh(v)

    def cosh(v: V): V = Cosh(v)

    def tanh(v: V): V = Tanh(v)

    def ln(v: V): V = Ln(v)

    def exp(v: V): V = Exp(v)

    def sqrt(v: V): V = Sqrt(v)

    def sum(v: V, axis: Int): V = Sum1(v, axis)

    def max(v: V, axis: Int): V = Max1(v, axis)

    def min(v: V, axis: Int): V = Min1(v, axis)

    // binary

    def pow(l: V, r: V): V = Pow(l, r)

    def max(l: V, r: V): V = Max2(l, r)

    def min(l: V, r: V): V = Min2(l, r)

    def dot(l: V, r: V): V = Dot(l, r)

    def matmul(l: V, r: V): V = MatMul(l, r)
  }

  object syntax {

    def where(cond: B, l: V, r: V): V = ElementwiseWhere(cond, l, r)

  }
}

