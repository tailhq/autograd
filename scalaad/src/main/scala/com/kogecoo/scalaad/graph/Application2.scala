package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
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
trait Application2[N <: Nat, L <: Nat, R <: Nat] extends V[N] {

  def shape: Shape[N]

  def l: V[L]

  def r: V[R]

}


// Binary Application

@throws[Exception]
case class Apply2[N <: Nat](l: V[N], r: V[N], op: BinaryOp) extends Application2[N, N, N] {

  if (l.shape != r.shape)
    throw new Exception(s"Shapes of the left (${l.shape}) and the right (${r.shape}) V must be equivalent.")

  def shape: Shape[N] = l.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl: V[N], dr: V[N]) = op.deriv[N, N](l, r)
    val fl: V[O] = l._forward[W, O](wrt)
    val fr: V[O] = r._forward[W, O](wrt)
    (fl :* dr) :+ (dl :* fr)
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = {
    val (dl: V[N], dr: V[N]) = op.deriv[N, N](l, r)
    l._reverse[G](adj  :* dr) ++ r._reverse[G](dl :* adj)
  }
}


@throws[Exception]
case class LeftShapedApply2[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) extends Application2[L, L, R] {

  if (l.shape.order <= r.shape.order)
    throw new Exception(s"The order of left (${l.shape}) must be larger than right's (${r.shape}).")

  if (l.shape.underlying.take(r.shape.order) != r.shape.underlying) {
    val expectedShape = l.shape.shrink(l.shape.underlying.indices.toList.drop(r.shape.order))
    val msg = s"Shapes of the left (${l.shape}) and the right (${r.shape}) are not aligned." +
              s" The right shape must be $expectedShape."
    throw new Exception(msg)
  }

  type RO <: Nat

  def shape: Shape[L] = l.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl, dr) = Unsafe.derivOp(l, r, op)
    val fl: V[O] = l._forward[W, O](wrt)
    val fr: V[RO] = r._forward[W, RO](wrt)
    (fl :* dr) :+ (dl :* fr)
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = {
    val (dl, dr) = Unsafe.derivOp(l, r, op)
    l._reverse[G](adj  :* dr) ++ r._reverse[G](dl :* adj)
  }

}


@throws[Exception]
case class RightShapedApply2[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) extends Application2[R, L, R] {

  if (l.shape.order >= r.shape.order)
    throw new Exception(s"The order of the left (${l.shape}) must be smaller than the right's (${r.shape}).")

  if (r.shape.underlying.take(l.shape.order) != l.shape.underlying) {
    val expectedShape = r.shape.shrink(r.shape.underlying.indices.toList.drop(l.shape.order))
    val msg = s"Shapes of the left (${l.shape}) and the right (${r.shape}) are not aligned." +
              s" The left shape must be $expectedShape."
    throw new Exception(msg)
  }

  type LO <: Nat

  def shape: Shape[R] = r.shape

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl, dr) = Unsafe.derivOp(l, r, op)
    val fl: V[LO] = l._forward[W, LO](wrt)
    val fr: V[O] = r._forward[W, O](wrt)
    (fl :* dr) :+ (dl :* fr)
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = {
    val (dl, dr) = Unsafe.derivOp(l, r, op)
    l._reverse[G](adj  :* dr) ++ r._reverse[G](dl :* adj)
  }

}


@throws[Exception]
case class Fold2[N <: Nat](l: V[Succ[N]], r: V[Succ[N]], op: BinaryFoldOp, axis: Int) extends Application2[N, Succ[N], Succ[N]] {

  if (l.shape != r.shape)
    throw new Exception(s"Shapes of the left (${l.shape}) and the right (${r.shape}) must be equivalent.")

  def shape: Shape[N] = l.shape.shrink(List(axis))

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl, dr) = op.deriv[Succ[N], Succ[N]](l, r)
    val fl = l._forward[W, Succ[O]](wrt)
    val fr = r._forward[W, Succ[O]](wrt)
    Unsafe.fold2_-(fl, dr, op, axis) :+ Unsafe.fold2_-(dl, fr, op, axis)
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = {
    val (dl, dr) = op.deriv(l, r)
    l._reverse[G](adj :* Unsafe.fold2_-(dl, r, op, axis)) ++ r._reverse[G](adj :* Unsafe.fold2_-(l, dr, op, axis))
  }

}

@throws[Exception]
case class LeftShapedFold2[L <: Nat, R <: Nat](l: V[Succ[L]], r: V[R], op: BinaryFoldOp, axis: Int) extends Application2[L, Succ[L], R] {

  if (l.shape.order <= r.shape.order)
    throw new Exception(s"The order of left (${l.shape}) must be larger than right's (${r.shape}).")

  if (l.shape.underlying.take(r.shape.order) != r.shape.underlying) {
    val expectedShape = l.shape.shrink(l.shape.underlying.indices.toList.drop(r.shape.order))
    val msg = s"Shapes of the left (${l.shape}) and the right (${r.shape}) are not aligned." +
              s" The right shape must be $expectedShape."
    throw new Exception(msg)
  }

  type RO <: Nat

  def shape: Shape[L] = l.shape.shrink(List(axis))

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl, dr) = op.deriv[Succ[L], R](l, r)
    val fl = l._forward[W, Succ[O]](wrt)
    val fr = r._forward[W, RO](wrt)
    Unsafe.fold2_-(fl, dr, op, axis) :+ Unsafe.fold2_-(dl, fr, op, axis)
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = {
    val (dl, dr) = op.deriv[Succ[L], R](l, r)
    l._reverse[G](adj :* Unsafe.fold2_-(dl, r, op, axis)) ++ r._reverse[G](adj :* Unsafe.fold2_-(l, dr, op, axis))
  }

}


@throws[Exception]
case class RightShapedFold2[L <: Nat, R <: Nat](l: V[L], r: V[Succ[R]], op: BinaryFoldOp, axis: Int) extends Application2[R, L, Succ[R]] {

  if (l.shape.order >= r.shape.order)
    throw new Exception(s"The order of the left (${l.shape}) must be smaller than the right's (${r.shape}).")

  if (r.shape.underlying.take(l.shape.order) != l.shape.underlying) {
    val expectedShape = r.shape.shrink(r.shape.underlying.indices.toList.drop(l.shape.order))
    val msg = s"Shapes of the left (${l.shape}) and the right (${r.shape}) are not aligned." +
              s" The left shape must be $expectedShape."
    throw new Exception(msg)
  }

  type LO <: Nat

  def shape: Shape[R] = l.shape.shrink(List(axis))

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl, dr) = op.deriv[L, Succ[R]](l, r)
    val fl = l._forward[W, LO](wrt)
    val fr = r._forward[W, Succ[R]](wrt)
    Unsafe.fold2_-(fl, dr, op, axis) :+ Unsafe.fold2_-(dl, fr, op, axis)
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = {
    val (dl, dr) = op.deriv[L, Succ[R]](l, r)
    l._reverse[G](adj :* Unsafe.fold2_-(dl, r, op, axis)) ++ r._reverse[G](adj :* Unsafe.fold2_-(l, dr, op, axis))
  }

}

