package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad._
import shapeless.Nat
import shapeless.Nat._2


// These constant Expr are shorthanded expression of Apply0[N](NullaryOp)

trait ConstBase[N <: Nat] extends ValueExpr[N] {

  def _forward[W <: Nat, O <: Nat](wrt: ValueExpr[W]): ValueExpr[O] = Zero(shape.extend(wrt.shape))

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = () // nop for constants

}


case class Zero[N <: Nat](shape: Shape[N]) extends ConstBase[N]

case class Half[N <: Nat](shape: Shape[N]) extends ConstBase[N]

case class One[N <: Nat](shape: Shape[N]) extends ConstBase[N]


case class Const[N <: Nat](v: Tensor[N]) extends ConstBase[N] { def shape: Shape[N] = v.shape }

case class Eye2[N <: Nat](shape: Shape[N]) extends ConstBase[N]
