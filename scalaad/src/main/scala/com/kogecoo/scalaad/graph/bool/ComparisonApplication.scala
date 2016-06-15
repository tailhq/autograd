package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.{Constraint, Shape}
import com.kogecoo.scalaad.graph.{V, ValueExpr}
import com.kogecoo.scalaad.op.bool.BinaryComparisonOp
import shapeless.Nat


// TODO: code-sharing with Application

/**
  * represents applying binary comparing operation, which takes 2 Exprs arguments.
  *
  * @tparam N a shape of this  BooleanExpr
  * @tparam L a shape of left  ValueExpr
  * @tparam R a shape of right ValueExpr
  */
trait ComparisonApplication2[N <: Nat, L <: Nat, R <: Nat] extends BooleanExpr[N] {

  def shape: Shape[N]

  def l: ValueExpr[L]

  def r: ValueExpr[R]

}


// Binary ComparisonApplication

object InferElementwise2C {

  def apply[O <: Nat, X <: Nat, Y <: Nat](a: V[X], b: V[Y], op: BinaryComparisonOp): BooleanExpr[O] = {
    (a, b) match {
      case _ if a.shape.order == b.shape.order => {
        val a_ = a.asInstanceOf[V[O]]
        val b_ = b.asInstanceOf[V[O]]
        Elementwise2C[O](a_, b_, op)
      }
      case _ if a.shape.order > b.shape.order => {
        val a_ = a.asInstanceOf[V[O]]
        BroadcastLeft2C[O, Y](a_, b, op)
      }
      case _ => {
        val b_ = b.asInstanceOf[V[O]]
        BroadcastRight2C[X, O](a, b_, op)
      }
    }
  }

}


@throws[Exception]
case class Elementwise2C[N <: Nat](l: V[N], r: V[N], op: BinaryComparisonOp) extends ComparisonApplication2[N, N, N] {

  Constraint.commonShape(l, r)

  def shape: Shape[N] = l.shape

}


@throws[Exception]
case class BroadcastLeft2C[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryComparisonOp) extends ComparisonApplication2[L, L, R] {

  Constraint.leftOrderBiggerThanRight(l, r)

  Constraint.broadcastableToLeft(l, r)

  def shape: Shape[L] = l.shape

}

@throws[Exception]
case class BroadcastRight2C[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryComparisonOp) extends ComparisonApplication2[R, L, R] {

  Constraint.rightOrderBiggerThanLeft(l, r)

  Constraint.broadcastableToRight(l, r)

  def shape: Shape[R] = r.shape

}

