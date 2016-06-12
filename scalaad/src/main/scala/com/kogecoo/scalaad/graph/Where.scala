package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.graph.bool.BooleanExpr
import shapeless.Nat


case class Where[N <:  Nat](cond: BooleanExpr[N], l: V[N], r: V[N]) extends ValueExpr[N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Where[O](cond, l._forward[W, O](wrt), r._forward[W, O](wrt))
  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    val lg: Grad[G] = l._reverse(g)
    val rg: Grad[G] = r._reverse(g)
    interpolateToGrad(lg, rg)

  }

  protected[this] def interpolateToGrad[G <: Nat](lg: Grad[G], rg: Grad[G]): Grad[G] = {
    val keys = lg.m.keySet | rg.m.keySet
    val m = keys.flatMap({ k =>
      val maybeValue = (lg.m.get(k), rg.m.get(k)) match {
        case (Some(a), Some(b)) => Some(Where[G](cond, a, b))
        case (Some(a), None)    => Some(If[G](cond, a))
        case (None,    Some(b)) => Some(NotIf[G](cond, b))
        case (None,    None)    => None
      }
      maybeValue.map((k, _))
    }).toMap
    new Grad[G](m)
  }
}


case class If[N <: Nat](cond: BooleanExpr[N], v: V[N]) extends ValueExpr[N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    If[O](cond, v._forward[W, O](wrt))
  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    val vg: Grad[G] = v._reverse(g)
    interpolateToGrad(vg)
  }

  protected[this] def interpolateToGrad[G <: Nat](vg: Grad[G]): Grad[G] = {
    val m = vg.m.map { case (k, v) => (k, If[G](cond, v)) }
    new Grad[G](m)
  }
}


case class NotIf[N <: Nat](cond: BooleanExpr[N], v: V[N]) extends ValueExpr[N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    NotIf[O](cond, v._forward[W, O](wrt))
  }

  def _reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    val vg: Grad[G] = v._reverse(g)
    interpolateToGrad(vg)
  }

  protected[this] def interpolateToGrad[G <: Nat](vg: Grad[G]): Grad[G] = {
    val m = vg.m.map { case (k, v) => (k, If[G](cond, v)) }
    new Grad[G](m)
  }
}
