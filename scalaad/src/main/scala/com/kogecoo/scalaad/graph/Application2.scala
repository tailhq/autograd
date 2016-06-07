package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{Add, BinaryOp, Mul, NullaryOp, UnaryOp, ZeroOp}
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
  protected[this] def add[O <: Nat, A <: Nat, B <: Nat](a: VE[A], b: VE[B]): VE[O] = applyOp(a, b, Add)

  protected[this] def mul[O <: Nat, A <: Nat, B <: Nat](a: VE[A], b: VE[B]): VE[O] = applyOp(a, b, Mul)

  // workaround Non type checking
  private[this] def applyOp[O <: Nat, A <: Nat, B <: Nat](a: VE[A], b: VE[B], op: BinaryOp): VE[O] = {
    (a, b) match {
      case _ if a.shape.order == b.shape.order => {
        val a_ = a.asInstanceOf[VE[O]]
        val b_ = b.asInstanceOf[VE[O]]
        Apply2[O](a_, b_, op)
      }
      case _ if a.shape.order > b.shape.order => {
        val a_ = a.asInstanceOf[VE[O]]
        ElementwiseLeft[O, B](a_, b, op)
      }
      case _ => {
        val b_ = b.asInstanceOf[VE[O]]
        ElementwiseRight[A, O](a, b_, op)
      }
    }
  }

}

/**
  * specialized Application2, its left and output Expr are the same type of shape.
  *
  * @tparam L a shape of left and output Expr
  * @tparam R a shape of right Expr
  */
trait LeftShapedApplication2[L <: Nat, R <: Nat] extends Application2[L, L, R] {
  def shape: Shape[L] = l.shape
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its right and output Expr are the same type of shape.
  *
  * @tparam L a shape of left Expr
  * @tparam R a shape of right and output Expr
  */
trait RightShapedApplication2[L <: Nat, R <: Nat] extends Application2[R, L, R] {
  def shape: Shape[R] = r.shape
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its left, right and output Expr are the same type of shape.
  *
  * @tparam N type of shape for left, right and output Expr
  */
trait CommonShapedApplication2[N <: Nat] extends Application2[N, N, N] {
  def shape: Shape[N] = l.shape
  def l: ValueExpr[N]
  def r: ValueExpr[N]
}

// Binary Application

case class Apply2[N <: Nat](l: VE[N], r: VE[N], op: BinaryOp) extends CommonShapedApplication2[N] {

  def _forward[W <: Nat, O <: Nat](wrt: VE[W]): VE[O] = {
    val (dl: VE[N], dr: VE[N]) = op.deriv[N, N](l, r)
    val fl: VE[O] = l._forward[W, O](wrt)
    val fr: VE[O] = r._forward[W, O](wrt)

    add(mul(fl, dr), mul(dl, fr))
  }

}


case class ElementwiseLeft[L <: Nat, R <: Nat](l: VE[L], r: VE[R], op: BinaryOp) extends LeftShapedApplication2[L, R] {

  type RO <: Nat

  def _forward[W <: Nat, O <: Nat](wrt: VE[W]): VE[O] = {
    val (dl: VE[L], dr: VE[R]) = op.deriv[L, R](l, r)
    val fl: VE[O] = l._forward[W, O](wrt)
    val fr: VE[RO] = r._forward[W, RO](wrt)
    add(mul(fl, dr), mul(dl, fr))
  }

}


case class ElementwiseRight[L <: Nat, R <: Nat](l: VE[L], r: VE[R], op: BinaryOp) extends RightShapedApplication2[L, R] {

  type LO <: Nat

  def _forward[W <: Nat, O <: Nat](wrt: VE[W]): VE[O] = {
    val (dl: VE[L], dr: VE[R]) = op.deriv[L, R](l, r)
    val fl: VE[LO] = l._forward[W, LO](wrt)
    val fr: VE[O] = r._forward[W, O](wrt)
    add(mul(fl, dr), mul(dl, fr))

  }

}


/*
case class Fold2[SI1 <: Nat, SI2 <: Nat](l: VE[SI1], r: VE[SI2], op: BinaryOp[S0, SI1, SI2]) extends Application2[S0, SI1, SI2] {

  def shape: Shape[S0] = Shape0()

}


case class Where[N <:  Nat](cond: BooleanExpr[N], l: VE[N], r: VE[N]) extends CommonShapedApplication2[N] {
}
*/
