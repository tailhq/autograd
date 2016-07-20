package scalaad

import scalaad.graph._


object Shorthands {

  object const {

    def zero(v: DExpr[Real]): DExpr[Real] = Zero(v.shape)

    def half(v: DExpr[Real]): DExpr[Real] = Half(v.shape)

    def one(v: DExpr[Real]): DExpr[Real] = One(v.shape)

    def two(v: DExpr[Real]): DExpr[Real] = Two(v.shape)

  }

  object math {

    // unary

    def sign(v: DExpr[Real]): DExpr[Real] = Sign(v)

    def sin(v: DExpr[Real]): DExpr[Real] = Sin(v)

    def cos(v: DExpr[Real]): DExpr[Real] = Cos(v)

    def tan(v: DExpr[Real]): DExpr[Real] = Tan(v)

    def asin(v: DExpr[Real]): DExpr[Real] = Asin(v)

    def acos(v: DExpr[Real]): DExpr[Real] = Acos(v)

    def atan(v: DExpr[Real]): DExpr[Real] = Atan(v)

    def sinh(v: DExpr[Real]): DExpr[Real] = Sinh(v)

    def cosh(v: DExpr[Real]): DExpr[Real] = Cosh(v)

    def tanh(v: DExpr[Real]): DExpr[Real] = Tanh(v)

    def ln(v: DExpr[Real]): DExpr[Real] = Ln(v)

    def exp(v: DExpr[Real]): DExpr[Real] = Exp(v)

    def sqrt(v: DExpr[Real]): DExpr[Real] = Sqrt(v)

    def sum(v: DExpr[Real], axis: Int): DExpr[Real] = Sum1(v, axis)

    def max(v: DExpr[Real], axis: Int): DExpr[Real] = Max1(v, axis)

    def min(v: DExpr[Real], axis: Int): DExpr[Real] = Min1(v, axis)

    // binary

    def pow(l: DExpr[Real], r: DExpr[Real]): DExpr[Real] = Pow(l, r)

    def max(l: DExpr[Real], r: DExpr[Real]): DExpr[Real] = Max2(l, r)

    def min(l: DExpr[Real], r: DExpr[Real]): DExpr[Real] = Min2(l, r)

    def dot(l: DExpr[Real], r: DExpr[Real]): DExpr[Real] = Dot(l, r)

    def matmul(l: DExpr[Real], r: DExpr[Real]): DExpr[Real] = MatMul(l, r)
  }

  object syntax {

    def where(cond: Expr[Bool], l: DExpr[Real], r: DExpr[Real]): DExpr[Real] = ElementwiseWhere(cond, l, r)

  }
}
