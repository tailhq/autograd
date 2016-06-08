package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.bool.BooleanExpr
import com.kogecoo.scalaad.op.{Add, BinaryFoldOp, BinaryOp, Mul, NullaryOp, UnaryOp, ZeroOp}
import shapeless.Nat
import shapeless.ops.nat.Sum


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

  // workaround Non type checking
  protected[this] def add[O <: Nat, A <: Nat, B <: Nat](a: V[A], b: V[B]): V[O] = applyOp(a, b, Add)

  protected[this] def mul[O <: Nat, A <: Nat, B <: Nat](a: V[A], b: V[B]): V[O] = applyOp(a, b, Mul)

  // workaround Non type checking
  private[this] def applyOp[O <: Nat, A <: Nat, B <: Nat](a: V[A], b: V[B], op: BinaryOp): V[O] = {
    (a, b) match {
      case _ if a.shape.order == b.shape.order => {
        val a_ = a.asInstanceOf[V[O]]
        val b_ = b.asInstanceOf[V[O]]
        Apply2[O](a_, b_, op)
      }
      case _ if a.shape.order > b.shape.order => {
        val a_ = a.asInstanceOf[V[O]]
        ElementwiseLeft[O, B](a_, b, op)
      }
      case _ => {
        val b_ = b.asInstanceOf[V[O]]
        ElementwiseRight[A, O](a, b_, op)
      }
    }
  }

}

// Binary Application

case class Apply2[N <: Nat](l: V[N], r: V[N], op: BinaryOp) extends Application2[N, N, N] {

  def shape: Shape[N] = l.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl: V[N], dr: V[N]) = op.deriv[N, N](l, r)
    val fl: V[O] = l._forward[W, O](wrt)
    val fr: V[O] = r._forward[W, O](wrt)

    add(mul(fl, dr), mul(dl, fr))
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = {
    val (dl: V[N], dr: V[N]) = op.deriv[N, N](l, r)
    l._reverse[G](g * dr)
    r._reverse[G](dl * g)
  }
}


case class ElementwiseLeft[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) extends Application2[L, L, R] {

  type RO <: Nat

  def shape: Shape[L] = l.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl: V[L], dr: V[R]) = op.deriv[L, R](l, r)
    val fl: V[O] = l._forward[W, O](wrt)
    val fr: V[RO] = r._forward[W, RO](wrt)
    add(mul(fl, dr), mul(dl, fr))
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = {
    val (dl: V[L], dr: V[R]) = op.deriv[L, R](l, r)
    l._reverse[G](g * dr)
    r._reverse[G](dl * g)
  }

}


case class ElementwiseRight[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) extends Application2[R, L, R] {

  type LO <: Nat

  def shape: Shape[R] = r.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl: V[L], dr: V[R]) = op.deriv[L, R](l, r)
    val fl: V[LO] = l._forward[W, LO](wrt)
    val fr: V[O] = r._forward[W, O](wrt)
    add(mul(fl, dr), mul(dl, fr))

  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = {
    val (dl: V[L], dr: V[R]) = op.deriv[L, R](l, r)
    l._reverse[G](g * dr)
    r._reverse[G](dl * g)
  }

}



case class Fold2[N <: Nat, L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryFoldOp[N, L, R]) extends Application2[N, L, R] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = { }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }

}


case class Where[N <:  Nat](cond: BooleanExpr[N], l: V[N], r: V[N]) extends Application2[N, N, N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = { }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }

}

