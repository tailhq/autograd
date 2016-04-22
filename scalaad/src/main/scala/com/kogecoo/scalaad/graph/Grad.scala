package com.kogecoo.scalaad.graph

import shapeless.Nat

import scala.Predef.{any2stringadd => _, _}
import scala.language.higherKinds


class Grad[G <: Nat](val m: Map[ValueExpr[_ <: Nat], ValueExpr[G]]) {

  def size: Int = m.size

  def apply(x: ValueExpr[_ <: Nat]): Option[ValueExpr[G]] = m.get(x)

  def ++(other: Grad[G]): Grad[G] = {
    val keys = this.m.keySet | other.m.keySet
    val m = (for {
      k <-  keys
      v1 =  this.m.get(k)
      v2 =  other.m.get(k)
      if v1.isDefined || v2.isDefined
    } yield {
      if (v1.isDefined && v2.isDefined) {
        (k, v1.get + v2.get)
      } else if (v1.isDefined) {
        (k, v1.get)
      } else {
        (k, v2.get)
      }
    }).toMap[ValueExpr[_ <: Nat], ValueExpr[G]]
    new Grad(m)
  }

}

object Grad {

  def apply[G <: Nat](p: ValueExpr[_ <: Nat], v: ValueExpr[G]): Grad[G] = {
    val m = Map[ValueExpr[_ <: Nat], ValueExpr[G]](p -> v)
    new Grad[G](m)
  }

  def empty[G <: Nat]: Grad[G] = {
    val m = Map.empty[ValueExpr[_ <: Nat], ValueExpr[G]]
    new Grad(m)
  }

}

