package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.graph.bool.{Apply2B, Apply2C, Apply2LeftC, Apply2RightC, ApplyLeftB, ApplyRightB}
import com.kogecoo.scalaad.op.{BinaryFoldOp, BinaryOp}
import com.kogecoo.scalaad.op.bool.{BinaryBooleanOp, BinaryComparisonOp}
import shapeless.{Nat, Succ}

import scala.language.existentials


// workaround: type unsafe
object Unsafe {

  def apply2[O <: Nat, X <: Nat, Y <: Nat](a: V[X], b: V[Y], op: BinaryOp): V[O] = {
    (a, b) match {
      case _ if a.shape.order == b.shape.order => {
        val a_ = a.asInstanceOf[V[O]]
        val b_ = b.asInstanceOf[V[O]]
        Apply2[O](a_, b_, op)
      }
      case _ if a.shape.order > b.shape.order => {
        val a_ = a.asInstanceOf[V[O]]
        LeftShapedApply2[O, Y](a_, b, op)
      }
      case _ => {
        val b_ = b.asInstanceOf[V[O]]
        RightShapedApply2[X, O](a, b_, op)
      }
    }
  }

  def apply2B[O <: Nat, X <: Nat, Y <: Nat](a: B[X], b: B[Y], op: BinaryBooleanOp): B[O] = {
    (a, b) match {
      case _ if a.shape.order == b.shape.order => {
        val a_ = a.asInstanceOf[B[O]]
        val b_ = b.asInstanceOf[B[O]]
        Apply2B[O](a_, b_, op)
      }
      case _ if a.shape.order > b.shape.order => {
        val a_ = a.asInstanceOf[B[O]]
        ApplyLeftB[O, Y](a_, b, op)
      }
      case _ => {
        val b_ = b.asInstanceOf[B[O]]
        ApplyRightB[X, O](a, b_, op)
      }
    }
  }

  def apply2C[O <: Nat, X <: Nat, Y <: Nat](a: V[X], b: V[Y], op: BinaryComparisonOp): B[O] = {
    (a, b) match {
      case _ if a.shape.order == b.shape.order => {
        val a_ = a.asInstanceOf[V[O]]
        val b_ = b.asInstanceOf[V[O]]
        Apply2C[O](a_, b_, op)
      }
      case _ if a.shape.order > b.shape.order => {
        val a_ = a.asInstanceOf[V[O]]
        Apply2LeftC[O, Y](a_, b, op)
      }
      case _ => {
        val b_ = b.asInstanceOf[V[O]]
        Apply2RightC[X, O](a, b_, op)
      }
    }
  }

  def fold2_-[O <: Nat, X <: Nat, Y <: Nat](a: V[X], b: V[Y], op: BinaryFoldOp, axis: Int): V[O] = {
    (a, b) match {
      case _ if a.shape.order == b.shape.order => {
        val a_ = a.asInstanceOf[V[Succ[O]]]
        val b_ = b.asInstanceOf[V[Succ[O]]]
        Fold2[O](a_, b_, op, axis)
      }
      case _ if a.shape.order > b.shape.order => {
        val a_ = a.asInstanceOf[V[Succ[O]]]
        LeftShapedFold2[O, Y](a_, b, op, axis)
      }
      case _ => {
        val b_ = b.asInstanceOf[V[Succ[O]]]
        RightShapedFold2[X, O](a, b_, op, axis)
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

  def where[O <: Nat, C <: Nat, X <: Nat, Y <: Nat](cond: B[C], x: V[X], y: V[Y]): V[O] = {
    (cond.shape.order, x.shape.order, y.shape.order) match {
      case (co, xo, yo) if co == xo && xo == yo => Where[O](cond.asInstanceOf[B[O]], x.asInstanceOf[V[O]], y.asInstanceOf[V[O]])
      case (co, xo, yo) if co <  xo && xo == yo => AsymmetricWhere[O, C](cond, x.asInstanceOf[V[O]], y.asInstanceOf[V[O]])
      case (co, xo, yo)                         => throw new Exception(s"Illegal shape order combination for Where operator ($co, $xo, $yo)")
    }
  }

   def if_[O <: Nat, C <: Nat, X <: Nat](cond: B[C], x: V[X]): V[O] = {
     (cond.shape.order, x.shape.order) match {
       case (co, xo) if co == xo => If[O](cond.asInstanceOf[B[O]], x.asInstanceOf[V[O]])
       case (co, xo) if co < xo  => AsymmetricIf[O, C](cond, x.asInstanceOf[V[O]])
       case (co, xo)             => throw new Exception(s"Illegal shape order combination for if operator ($co, $xo)")
     }
   }

   def notif[O <: Nat, C <: Nat, X <: Nat](cond: B[C], x: V[X]): V[O] = {
    (cond.shape.order, x.shape.order) match {
      case (co, xo) if co == xo => NotIf[O](cond.asInstanceOf[B[O]], x.asInstanceOf[V[O]])
      case (co, xo) if co <  xo => AsymmetricNotIf[O, C](cond, x.asInstanceOf[V[O]])
      case (co, xo)             => throw new Exception(s"Illegal shape order combination for notif operator ($co, $xo)")
    }
  }

}
