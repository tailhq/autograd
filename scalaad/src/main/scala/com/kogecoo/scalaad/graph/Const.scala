package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad._
import shapeless.Nat
import shapeless.Nat._2


trait ConstBase[N <: Nat] extends ValueExpr[N] {

  def _forward[W <: Nat, O <: Nat](wrt: ValueExpr[W]): ValueExpr[O] = Zero()
}


case class Zero[N <: Nat]() extends ConstBase[N]

case class Half[N <: Nat]() extends ConstBase[N]

case class One[N <: Nat](shape: Shape[N]) extends ConstBase[N]


case class Const[N <: Nat](v: Tensor[N]) extends ConstBase[N] {

  def shape: Shape[N] = v.shape

}

case class Eye2(shape: Shape[_2]) extends ConstBase[_2]
