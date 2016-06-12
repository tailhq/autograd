package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{UnaryExpandOp, UnaryFoldOp, UnaryOp}
import shapeless.{Nat, Succ}
import shapeless.Nat.{_1, _2}


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

  def _forward[W <: Nat](wrt: ValueExpr[W]): ValueExpr[N] = {
    v._forward[W](wrt) * op.deriv[N](v)
  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    v._reverse[G](g :* op.deriv[N](v))
  }

}


/**
  *
  * This is a particular variation of Apply that is accompanied by a reduction of 1 order of input's shape.
  * i.e. Here we want to express: V[I] => V[I-X] == V[N]
  *
  */
abstract class Fold1[N <: Nat, I <: Nat](v: V[I], op: UnaryFoldOp) extends Application1[N, I]


case class Fold1_-[N <: Nat](v: V[Succ[N]], op: UnaryFoldOp, axis: Int) extends Fold1[N, Succ[N]](v, op) {

  def shape: Shape[N] = v.shape.shrink(List(axis))

  def _forward[W <: Nat](wrt: ValueExpr[W]): ValueExpr[N] = {
    Fold1_-[N](v._forward[W](wrt), op, axis)
  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    v._reverse[G](g :* Fold1_-[N](op.deriv[Succ[N]](v), op, axis))
  }

}


case class Fold1_--[N <: Nat](v: V[Succ[Succ[N]]], op: UnaryFoldOp, axis1: Int, axis2: Int) extends Fold1[N, Succ[Succ[N]]](v, op) {

  def shape: Shape[N] = v.shape.shrink(List(axis1, axis2))

  def _forward[W <: Nat](wrt: ValueExpr[W]): ValueExpr[N] = {
    Fold1_--[N](v._forward[W](wrt), op, axis1, axis2)
  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    v._reverse[G](g :* Fold1_--(op.deriv[Succ[Succ[N]]](v), op, axis1, axis2))
  }

}


abstract class Expand1[N <: Nat, I <: Nat](v: V[I], op: UnaryExpandOp) extends Application1[N, I]


case class Expand1_+[N <: Nat, I <: Nat](v: V[I], op: UnaryExpandOp, s: Shape[_1]) extends Expand1[N, I](v, op) {

  def shape: Shape[N] = v.shape.extend(s)

  def _forward[W <: Nat](wrt: ValueExpr[W]): ValueExpr[N] = {
    Expand1_+[N, I](v._forward[W](wrt), op, s)
  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    v._reverse[G](g :* Expand1_+[N, I](op.deriv[I](v), op, s))
  }

}


case class Expand1_++[N <: Nat, I <: Nat](v: V[I], op: UnaryExpandOp, s: Shape[_2]) extends Expand1[N, I](v, op) {

  def shape: Shape[N] = v.shape.extend(s)

  def _forward[W <: Nat](wrt: ValueExpr[W]): ValueExpr[N] = {
    Expand1_++[N, I](v._forward[W](wrt), op, s)
  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    v._reverse[G](g :* Expand1_++[N, I](op.deriv[I](v), op, s))
  }

}

