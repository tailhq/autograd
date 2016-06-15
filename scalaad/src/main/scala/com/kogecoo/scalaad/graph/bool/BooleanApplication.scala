package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.{Constraint, Shape}
import com.kogecoo.scalaad.graph.B
import com.kogecoo.scalaad.op.bool.{BinaryBooleanOp, UnaryBooleanExpandOp, UnaryBooleanOp}
import shapeless.Nat


// TODO: code-sharing with Application

/**
  * represents applying binary operation, which takes 2 Exprs arguments.
 *
  * @tparam N a shape of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right Expr
  */
trait BooleanApplication2[N <: Nat, L <: Nat, R <: Nat] extends B[N] {

  def l: B[L]

  def r: B[R]
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
 *
  * @tparam N type of shape for output Expr
  * @tparam I type of shape for argument Expr
  */
trait BooleanApplication1[N <: Nat, I <: Nat] extends B[N] {

  def v: B[I]

}

// Unary BooleanApplication

case class Apply1B[N <: Nat](v: B[N], op: UnaryBooleanOp) extends BooleanApplication1[N, N] {

  def shape: Shape[N] = v.shape

}


object InferElementwise2B {

  def apply[O <: Nat, X <: Nat, Y <: Nat](a: B[X], b: B[Y], op: BinaryBooleanOp): B[O] = {
    (a, b) match {
      case _ if a.shape.order == b.shape.order => {
        val a_ = a.asInstanceOf[B[O]]
        val b_ = b.asInstanceOf[B[O]]
        Elementwise2B[O](a_, b_, op)
      }
      case _ if a.shape.order > b.shape.order => {
        val a_ = a.asInstanceOf[B[O]]
        BroadcastLeft2B[O, Y](a_, b, op)
      }
      case _ => {
        val b_ = b.asInstanceOf[B[O]]
        BroadcastRight2B[X, O](a, b_, op)
      }
    }
  }

}


@throws[Exception]
case class Elementwise2B[N <: Nat](l: B[N], r: B[N], op: BinaryBooleanOp) extends BooleanApplication2[N, N, N] {

  Constraint.commonShape(l, r)

  def shape: Shape[N] = l.shape
}


@throws[Exception]
case class BroadcastLeft2B[L <: Nat, R <: Nat](l: B[L], r: B[R], op: BinaryBooleanOp) extends BooleanApplication2[L, L, R] {

  Constraint.leftOrderBiggerThanRight(l, r)

  Constraint.broadcastableToLeft(l, r)

  def shape: Shape[L] = l.shape

}


@throws[Exception]
case class BroadcastRight2B[L <: Nat, R <: Nat](l: B[L], r: B[R], op: BinaryBooleanOp) extends BooleanApplication2[R, L, R] {

  Constraint.rightOrderBiggerThanLeft(l, r)

  Constraint.broadcastableToRight(l, r)

  def shape: Shape[R] = r.shape

}


case class Expand1B[N <: Nat, L <: Nat, R <: Nat](v: B[L], s: Shape[R], op: UnaryBooleanExpandOp) extends BooleanApplication1[N, L] {

  def shape: Shape[N] = v.shape.extend(s)

}
