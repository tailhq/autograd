package scalaad

import scalaad.graph._


object Shorthands {

  object const {

    def zero(v: DExpr): DExpr = Zero(v.shape)

    def half(v: DExpr): DExpr = Half(v.shape)

    def one(v: DExpr): DExpr = One(v.shape)

    def two(v: DExpr): DExpr = Two(v.shape)

  }

  object math {

    // unary

    def sign(v: DExpr): DExpr = Sign(v)

    def sin(v: DExpr): DExpr = Sin(v)

    def cos(v: DExpr): DExpr = Cos(v)

    def tan(v: DExpr): DExpr = Tan(v)

    def asin(v: DExpr): DExpr = Asin(v)

    def acos(v: DExpr): DExpr = Acos(v)

    def atan(v: DExpr): DExpr = Atan(v)

    def sinh(v: DExpr): DExpr = Sinh(v)

    def cosh(v: DExpr): DExpr = Cosh(v)

    def tanh(v: DExpr): DExpr = Tanh(v)

    def ln(v: DExpr): DExpr = Ln(v)

    def exp(v: DExpr): DExpr = Exp(v)

    def sqrt(v: DExpr): DExpr = Sqrt(v)

    def sum(v: DExpr, axis: Int): DExpr = Sum1(v, axis)

    def max(v: DExpr, axis: Int): DExpr = Max1(v, axis)

    def min(v: DExpr, axis: Int): DExpr = Min1(v, axis)

    // binary

    def pow(l: DExpr, r: DExpr): DExpr = Pow(l, r)

    def max(l: DExpr, r: DExpr): DExpr = Max2(l, r)

    def min(l: DExpr, r: DExpr): DExpr = Min2(l, r)

    def dot(l: DExpr, r: DExpr): DExpr = Dot(l, r)

    def matmul(l: DExpr, r: DExpr): DExpr = MatMul(l, r)
  }

  object syntax {

    def where(cond: Expr, l: DExpr, r: DExpr): DExpr = ElementwiseWhere(cond, l, r)

  }
}
