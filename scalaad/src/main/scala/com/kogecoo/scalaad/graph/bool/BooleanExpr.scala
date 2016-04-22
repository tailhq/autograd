package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph.{B, Expr, Unsafe}
import com.kogecoo.scalaad.op.bool.{And, ExpandEvery, Not, Or}
import shapeless.Nat


trait BooleanExpr[N <: Nat] extends Expr[N] {

  def eval[R](implicit E: Eval[BooleanExpr[N], R]): R = E.eval(this)

}


object BooleanExpr {

  implicit class RichValueExpr[N <: Nat](val self: B[N]) extends AnyVal {

    def &(rhs: B[N]): B[N] = Apply2B[N](self, rhs, And)
    def |(rhs: B[N]): B[N] = Apply2B[N](self, rhs, Or)

    def &:<[M <: Nat](rhs: B[M]): B[M] = ApplyRightB[N, M](self, rhs, And)
    def |:<[M <: Nat](rhs: B[M]): B[M] = ApplyRightB[N, M](self, rhs, Or)

    def &:>[M <: Nat](rhs: B[M]): B[M]= ApplyRightB[N, M](self, rhs, And)
    def |:>[M <: Nat](rhs: B[M]): B[M]= ApplyRightB[N, M](self, rhs, Or)

    def :&[M <: Nat, O <: Nat](rhs: B[M]): B[O]= Unsafe.apply2B[O, N, M](self, rhs, And)
    def :|[M <: Nat, O <: Nat](rhs: B[M]): B[O]= Unsafe.apply2B[O, N, M](self, rhs, Or)

    def unary_!(): B[N] = Apply1B[N](self, Not)

    def expand[M <: Nat, O <: Nat](s: Shape[M]): B[O] = Expand1B[O, N, M](self, s, ExpandEvery)
  }

}

