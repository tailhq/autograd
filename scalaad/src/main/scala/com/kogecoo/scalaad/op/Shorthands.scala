package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Apply1, Fold1, Half, One, Two, Unsafe, V, Zero}
import shapeless.{Nat, Succ}


object Shorthands {

  object Const {

    def zero[N <: Nat](v: V[N]): V[N] = Zero[N](v.shape)

    def half[N <: Nat](v: V[N]): V[N] = Half[N](v.shape)

    def one[N <: Nat](v: V[N]): V[N] = One[N](v.shape)

    def two[N <: Nat](v: V[N]): V[N] = Two[N](v.shape)

  }

  object Math {

    // unary

    def sign[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Sign)

    def sin[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Sin)

    def cos[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Cos)

    def tan[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Tan)

    def asin[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Asin)

    def acos[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Acos)

    def atan[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Atan)

    def sinh[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Sinh)

    def cosh[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Cosh)

    def tanh[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Tanh)

    def ln[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Ln)

    def exp[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Exp)

    def sqrt[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Sqrt)

    def sum[N <: Nat](v: V[Succ[N]], axis: Int): V[N] = Fold1[N](v, Sum1, axis)

    // binary

    def pow[L <: Nat, R <: Nat](l: V[L], r: V[R]): V[_ <: Nat] = Unsafe.apply2(l, r, Pow)


  }
}

