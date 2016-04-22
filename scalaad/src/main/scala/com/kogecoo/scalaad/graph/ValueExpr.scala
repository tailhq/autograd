package com.kogecoo.scalaad.graph


import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph.bool.{Apply2C, Apply2LeftC, Apply2RightC}
import com.kogecoo.scalaad.op.bool.{Eq, Gt, Gte, Lt, Lte, Neq}
import com.kogecoo.scalaad.op.{Add, Div, Mul, Neg, Pos, Sub}
import shapeless.Nat
import shapeless.ops.nat.Sum



trait ValueExpr[N <: Nat]  extends Expr[N]{

  final def forward[W <: Nat, O <: Nat](wrt: ValueExpr[W])(implicit s: Sum.Aux[N, W, O]): ValueExpr[O] = {
    _forward[W, s.Out](wrt)
  }

  final def reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = _reverse(g)

  def eval[R](implicit E: Eval[ValueExpr[N], R]): R = E.eval(this)

  def _forward[W <: Nat, O <: Nat](wrt: ValueExpr[W]): ValueExpr[O]

  def _reverse[G <: Nat](adj: ValueExpr[G]): Grad[G]

}


object ValueExpr {

  implicit class RichValueExpr[N <: Nat](val self: V[N]) extends AnyVal {

    def unary_+(): V[N] = Apply1[N](self, Pos)
    def unary_-(): V[N] = Apply1[N](self, Neg)

    def +(rhs: V[N]): V[N] = Apply2[N](self, rhs, Add)
    def -(rhs: V[N]): V[N] = Apply2[N](self, rhs, Sub)
    def *(rhs: V[N]): V[N] = Apply2[N](self, rhs, Mul)
    def /(rhs: V[N]): V[N] = Apply2[N](self, rhs, Div)

    def +:>[M <: Nat](rhs: V[M]): V[N] = LeftShapedApply2[N, M](self, rhs, Add)
    def -:>[M <: Nat](rhs: V[M]): V[N] = LeftShapedApply2[N, M](self, rhs, Sub)
    def *:>[M <: Nat](rhs: V[M]): V[N] = LeftShapedApply2[N, M](self, rhs, Mul)
    def /:>[M <: Nat](rhs: V[M]): V[N] = LeftShapedApply2[N, M](self, rhs, Div)

    def +:<[M <: Nat](rhs: V[M]): V[M] = RightShapedApply2[N, M](self, rhs, Add)
    def -:<[M <: Nat](rhs: V[M]): V[M] = RightShapedApply2[N, M](self, rhs, Sub)
    def *:<[M <: Nat](rhs: V[M]): V[M] = RightShapedApply2[N, M](self, rhs, Mul)
    def /:<[M <: Nat](rhs: V[M]): V[M] = RightShapedApply2[N, M](self, rhs, Div)

    def ==(rhs: V[N]): B[N] = Apply2C(self, rhs, Eq)
    def !=(rhs: V[N]): B[N] = Apply2C(self, rhs, Neq)
    def < (rhs: V[N]): B[N] = Apply2C(self, rhs, Lt)
    def <=(rhs: V[N]): B[N] = Apply2C(self, rhs, Lte)
    def > (rhs: V[N]): B[N] = Apply2C(self, rhs, Gt)
    def >=(rhs: V[N]): B[N] = Apply2C(self, rhs, Gte)

    def ==:<[M <: Nat](rhs: V[M]): B[N] = Apply2LeftC(self, rhs, Eq)
    def !=:<[M <: Nat](rhs: V[M]): B[N] = Apply2LeftC(self, rhs, Neq)
    def <:< [M <: Nat](rhs: V[M]): B[N] = Apply2LeftC(self, rhs, Lt)
    def <=:<[M <: Nat](rhs: V[M]): B[N] = Apply2LeftC(self, rhs, Lte)
    def >:< [M <: Nat](rhs: V[M]): B[N] = Apply2LeftC(self, rhs, Gt)
    def >=:<[M <: Nat](rhs: V[M]): B[N] = Apply2LeftC(self, rhs, Gte)

    def ==:>[M <: Nat](rhs: V[M]): B[M] = Apply2RightC(self, rhs, Eq)
    def !=:>[M <: Nat](rhs: V[M]): B[M] = Apply2RightC(self, rhs, Neq)
    def <:> [M <: Nat](rhs: V[M]): B[M] = Apply2RightC(self, rhs, Lt)
    def <=:>[M <: Nat](rhs: V[M]): B[M] = Apply2RightC(self, rhs, Lte)
    def >:> [M <: Nat](rhs: V[M]): B[M] = Apply2RightC(self, rhs, Gt)
    def >=:>[M <: Nat](rhs: V[M]): B[M] = Apply2RightC(self, rhs, Gte)

    // type unsafe operations
    def :+[M <: Nat, O <: Nat](rhs: V[M]): V[O] = Unsafe.apply2[O, N, M](self, rhs, Add)
    def :-[M <: Nat, O <: Nat](rhs: V[M]): V[O] = Unsafe.apply2[O, N, M](self, rhs, Sub)
    def :*[M <: Nat, O <: Nat](rhs: V[M]): V[O] = Unsafe.apply2[O, N, M](self, rhs, Mul)
    def :/[M <: Nat, O <: Nat](rhs: V[M]): V[O] = Unsafe.apply2[O, N, M](self, rhs, Div)

    def :==[M <: Nat, O <: Nat](rhs: V[M]): B[O] = Unsafe.apply2C(self, rhs, Eq)
    def :!=[M <: Nat, O <: Nat](rhs: V[M]): B[O] = Unsafe.apply2C(self, rhs, Neq)
    def :< [M <: Nat, O <: Nat](rhs: V[M]): B[O] = Unsafe.apply2C(self, rhs, Lt)
    def :<=[M <: Nat, O <: Nat](rhs: V[M]): B[O] = Unsafe.apply2C(self, rhs, Lte)
    def :> [M <: Nat, O <: Nat](rhs: V[M]): B[O] = Unsafe.apply2C(self, rhs, Gt)
    def :>=[M <: Nat, O <: Nat](rhs: V[M]): B[O] = Unsafe.apply2C(self, rhs, Gte)
  }

}

