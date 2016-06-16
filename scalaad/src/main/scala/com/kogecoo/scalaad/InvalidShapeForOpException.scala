package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph.{Expr, V, ValueExpr}
import com.kogecoo.scalaad.op.Op
import shapeless.Nat


class InvalidShapeForOpException(a: ValueExpr[_], b: ValueExpr[_], op: String, additionalMsg: Option[String])
  extends Exception(
    s"$op cannot applicable for variables with shape pair ${a.shape} and ${b.shape}\n" + additionalMsg.getOrElse("")
  )


object Constraint {

  @throws[Exception]
  def satisfy(cond: Boolean, msg: String): Unit = {
    if (!cond) throw new Exception(msg)
  }

  @throws[Exception]
  def commonShape[L <: Nat, R <: Nat](l: Expr[L], r: Expr[R]): Unit = satisfy(
    l.shape.order == r.shape.order && l.shape == r.shape,
    s"Shapes (${l.shape}) and (${r.shape}) must be equivalent."
  )

  @throws[Exception]
  def leftOrderBiggerThanRight[L <: Nat, R <: Nat](l: Expr[L], r: Expr[R]): Unit = satisfy(
    l.shape.order > r.shape.order,
    s"The order of the left (${l.shape}) must be bigger than the right's (${r.shape})."
  )

  @throws[Exception]
  def rightOrderBiggerThanLeft[L <: Nat, R <: Nat](l: Expr[L], r: Expr[R]): Unit = satisfy(
    l.shape.order < r.shape.order,
    s"The order of the left (${l.shape}) must be smaller than the right's (${r.shape})."
  )

  @throws[Exception]
  def broadcastableToLeft[L <: Nat, R <: Nat](l: Expr[L], r: Expr[R]): Unit = satisfy(
    l.shape.underlying.take(r.shape.order) == r.shape.underlying,
    {
      val hint = l.shape.shrink(l.shape.underlying.indices.toList.drop(r.shape.order))
      s"Broadcast cannot perform for shape pair (${l.shape}) and (${r.shape}). Maybe the left shape must be $hint."
    }
  )

  @throws[Exception]
  def broadcastableToRight[L <: Nat, R <: Nat](l: Expr[L], r: Expr[R]): Unit = satisfy(
    r.shape.underlying.take(l.shape.order) == l.shape.underlying,
    {
      val hint = r.shape.shrink(r.shape.underlying.indices.toList.drop(l.shape.order))
      s"Broadcast cannot perform for shape pair (${l.shape}) and (${r.shape}). Maybe the right shape must be $hint."
    }
  )

  @throws[Exception]
  def validAxis[N <: Nat](e: Expr[N], axis: Int): Unit = satisfy(
    axis < e.shape.order,
    s"Axis $axis exceeds the dimension of Expr ($e)"
  )

  @throws[Exception]
  def foldable[N <: Nat](e: Expr[N]): Unit = satisfy(
    e.shape.order > 0,
    s"Fold cannot perform for the order of Expr $e. It must be > 0"
  )

}