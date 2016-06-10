package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.bool.BooleanExpr
import com.kogecoo.scalaad.op.{BinaryFoldOp, BinaryOp}
import shapeless.{Nat, Succ}

import scala.language.existentials


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


object Unsafe {

  // workaround: type unsafe
  def apply2[O <: Nat, A <: Nat, B <: Nat](a: V[A], b: V[B], op: BinaryOp): V[O] = {
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

  def derivOp[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) = {

    val lo = l.shape.order
    val ro = r.shape.order
    val (a: V[_], b: V[_]) = op.deriv[L, R](l, r)

    val dl = a.shape.order match {
      case o if o == lo => a.asInstanceOf[V[L]]
      case o if o == ro => a.asInstanceOf[V[R]]
      case o            => throw new Exception(s"unknown shape $o for Application2($l, $r, $op)")
    }
    val dr = b.shape.order match {
      case o if o == lo => b.asInstanceOf[V[L]]
      case o if o == ro => b.asInstanceOf[V[R]]
      case o            => throw new Exception(s"unknown shape $o for Application2($l, $r, $op)")
    }
    (dl, dr)
  }

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

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    val (dl: V[N], dr: V[N]) = op.deriv[N, N](l, r)
    l._reverse[G](g  :* dr) ++ r._reverse[G](dl :* g)
  }
}


case class ElementwiseLeft[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) extends Application2[L, L, R] {

  type RO <: Nat

  def shape: Shape[L] = l.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl, dr) = Unsafe.derivOp(l, r, op)
    val fl: V[O] = l._forward[W, O](wrt)
    val fr: V[RO] = r._forward[W, RO](wrt)
    (fl :* dr) :+ (dl :* fr)
  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    val (dl, dr) = Unsafe.derivOp(l, r, op)
    l._reverse[G](g  :* dr) ++ r._reverse[G](dl :* g)
  }

}


case class ElementwiseRight[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) extends Application2[R, L, R] {

  type LO <: Nat

  def shape: Shape[R] = r.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl, dr) = Unsafe.derivOp(l, r, op)
    val fl: V[LO] = l._forward[W, LO](wrt)
    val fr: V[O] = r._forward[W, O](wrt)
    (fl :* dr) :+ (dl :* fr)
  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    val (dl, dr) = Unsafe.derivOp(l, r, op)
    l._reverse[G](g  :* dr) ++ r._reverse[G](dl :* g)
  }

}



// Experimental
// Fold seems to be broken
abstract class Fold2[N <: Nat, L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryFoldOp) extends Application2[N, L, R]


case class Fold2_-[N <: Nat](l: V[Succ[N]], r: V[Succ[N]], op: BinaryFoldOp, axis: Int) extends Application2[N, N, N] {

  def shape: Shape[N] = l.shape.shrink(List(axis))

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val fl = l._forward[W, Succ[O]](wrt)
    val fr = r._forward[W, Succ[O]](wrt)
    Fold2_-[O](fl, fr, op, axis)
  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    val (dl, dr) = op.deriv[Succ[N]](l, r)
    // FIXME l._reverse[G](Fold2_-[N](g, dr, op, axis)), r._reverse[G](Fold2_-[N](dl, g, op, axis))
  }

}


case class Where[N <:  Nat](cond: BooleanExpr[N], l: V[N], r: V[N]) extends Application2[N, N, N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {

  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
   // FIXME Where[N](cond, l._reverse[G](g), r._reverse[G](g))
  }

}

