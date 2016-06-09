package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{UnaryExpandOp, UnaryFoldOp, UnaryOp}
import shapeless.{Nat, Succ}


/**
  * represents applying unary operation, which takes 1 Expr as argument.
  *
  * @tparam N type of shape for output Expr
  * @tparam I type of shape for argument Expr
  */
trait Application1[N <: Nat, I <: Nat] extends ValueExpr[N] {

  def shape: Shape[N]

  def v: ValueExpr[I]

}


// Unary Application

case class Apply1[N <: Nat](v: V[N], op: UnaryOp) extends Application1[N, N] {

  def shape: Shape[N] = v.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    v._forward[W, O](wrt) :* op.deriv[N](v)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = {
    v._reverse[G](g :* op.deriv[N](v), builder)
  }

}


/**
  *
  * This is a particular variation of Apply that is accompanied by a reduction of 1 order of input's shape.
  * i.e. Here we want to express: V[I] => V[I-X] == V[N]
  */
abstract class Fold1[N <: Nat, I <: Nat](v: V[I], op: UnaryFoldOp) extends Application1[N, I]


case class Fold1_1[N <: Nat](v: V[Succ[N]], op: UnaryFoldOp, axis: Int) extends Fold1[N, Succ[N]](v, op) {

  def shape: Shape[N] = v.shape.shrink(List(axis))

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Fold1_1[O](v._forward[W, Succ[O]](wrt), op, axis)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit

}


case class Fold1_2[N <: Nat](v: V[Succ[Succ[N]]], op: UnaryFoldOp, axis1: Int, axis2: Int) extends Fold1[N, Succ[Succ[N]]](v, op) {

  def shape: Shape[N] = v.shape.shrink(List(axis1, axis2))

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Fold1_2[O](v._forward[W, Succ[Succ[O]]](wrt), op, axis1, axis2)
  }

}


abstract class Expand1[N <: Nat, I <: Nat](v: V[I], op: UnaryExpandOp) extends Application1[N, I]


case class Expand1_1[N <: Nat, I <: Nat](v: V[I], op: UnaryExpandOp) extends Expand1[N, I](v, op) {

  type I_ <: Nat

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Expand1_1[O, I_](v._forward[W, I_](wrt), op)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }
}


case class Expand1_2[N <: Nat, I <: Nat](v: V[I], op: UnaryExpandOp) extends Expand1[N, I](v, op) {

  type I_ <: Nat

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Expand1_1[O, I_](v._forward[W, I_](wrt), op)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }

}

