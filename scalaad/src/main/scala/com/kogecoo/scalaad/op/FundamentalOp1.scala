package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Elementwise1, One, ValueExpr}
import shapeless.Nat


case object Pos extends UnaryOp {

  override def deriv[N <: Nat](v: ValueExpr[N]): ValueExpr[N] = Elementwise1(One(v.shape), Pos)

}

case object Neg extends UnaryOp {

  override def deriv[N <: Nat](v: ValueExpr[N]): ValueExpr[N] = Elementwise1(One(v.shape), Neg)

}

case object Identity extends UnaryOp {

  override def deriv[N <: Nat](v: ValueExpr[N]): ValueExpr[N] = Elementwise1(One(v.shape), Identity)

}

case object Sign extends UnaryOp {

  override def deriv[N <: Nat](v: ValueExpr[N]): ValueExpr[N] = Elementwise1(One(v.shape), Sign)

}
