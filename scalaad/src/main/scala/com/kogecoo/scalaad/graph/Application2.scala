package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.bool.BooleanExpr
import com.kogecoo.scalaad.op.{Add, BinaryFoldOp, BinaryOp, Mul}
import shapeless.Nat


/**
  * represents applying binary operation, which takes 2 Exprs arguments.
  *
  * @tparam N a shape of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right Expr
  */
trait Application2[N <: Nat, L <: Nat, R <: Nat] extends ValueExpr[N] {

  def shape: Shape[N]

  def l: ValueExpr[L]

  def r: ValueExpr[R]

}

// Binary Application

case class Apply2[N <: Nat](l: V[N], r: V[N], op: BinaryOp) extends Application2[N, N, N] {

  def shape: Shape[N] = l.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl: V[N], dr: V[N]) = op.deriv[N, N](l, r)
    val fl: V[O] = l._forward[W, O](wrt)
    val fr: V[O] = r._forward[W, O](wrt)

    (fl :* dr) :+ (dl :* fr)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = {
    val (dl: V[N], dr: V[N]) = op.deriv[N, N](l, r)
    l._reverse[G](g  :* dr, builder)
    r._reverse[G](dl :* g,  builder)
  }
}


case class ElementwiseLeft[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) extends Application2[L, L, R] {

  type RO <: Nat

  def shape: Shape[L] = l.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl: V[L], dr: V[R]) = op.deriv[L, R](l, r)
    val fl: V[O] = l._forward[W, O](wrt)
    val fr: V[RO] = r._forward[W, RO](wrt)
    (fl :* dr) :+ (dl :* fr)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = {
    val (dl: V[L], dr: V[R]) = op.deriv[L, R](l, r)
    l._reverse[G](g  :* dr, builder)
    r._reverse[G](dl :* g,  builder)
  }

}


case class ElementwiseRight[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) extends Application2[R, L, R] {

  type LO <: Nat

  def shape: Shape[R] = r.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl: V[L], dr: V[R]) = op.deriv[L, R](l, r)
    val fl: V[LO] = l._forward[W, LO](wrt)
    val fr: V[O] = r._forward[W, O](wrt)
    (fl :* dr) :+ (dl :* fr)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = {
    val (dl: V[L], dr: V[R]) = op.deriv[L, R](l, r)
    l._reverse[G](g  :* dr, builder)
    r._reverse[G](dl :* g,  builder)
  }

}


/*
case class Fold2[N <: Nat, L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryFoldOp[N, L, R]) extends Application2[N, L, R] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = { }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }

}


case class Where[N <:  Nat](cond: BooleanExpr[N], l: V[N], r: V[N]) extends Application2[N, N, N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = { }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }

}
*/
