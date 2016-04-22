package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.Shape
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

case class Apply2C[S <: Nat](l: V[S], r: V[S], op: BinaryComparisonOp) extends ComparisonApplication2[S, S, S] {

  def shape: Shape[S] = l.shape

}

case class Apply2LeftC[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryComparisonOp) extends ComparisonApplication2[L, L, R] {

  def shape: Shape[L] = l.shape

}

case class Apply2RightC[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryComparisonOp) extends ComparisonApplication2[R, L, R] {

  def shape: Shape[R] = r.shape

}

