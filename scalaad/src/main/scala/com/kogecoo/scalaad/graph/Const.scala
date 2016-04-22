package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad._
import shapeless.Nat
import shapeless.Nat._1


// These constant Expr are shorthanded expression of Apply0[N](NullaryOp)

trait ConstBase[N <: Nat] extends V[N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = Zero(shape.extend(wrt.shape))

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = Grad.empty[G]

}


case class Zero[N <: Nat](shape: Shape[N]) extends ConstBase[N]

case class Half[N <: Nat](shape: Shape[N]) extends ConstBase[N]

case class One[N <: Nat](shape: Shape[N]) extends ConstBase[N]

case class Two[N <: Nat](shape: Shape[N]) extends ConstBase[N]


case class Const[N <: Nat](v: Tensor[N]) extends ConstBase[N] { def shape: Shape[N] = v.shape }

@throws[Exception]
case class Eye[N <: Nat](shape: Shape[N]) extends ConstBase[N]


@throws[Exception]
case class Diag[N <: Nat](diag: Tensor[_1], order: Int) extends ConstBase[N] {

  if (shape.order < 1) throw new Exception(s"Eye requires Shape with order N >= 2")

  def shape: Shape[N] = Shape[N](List.fill(order)(diag.shape.at(0)))

}

