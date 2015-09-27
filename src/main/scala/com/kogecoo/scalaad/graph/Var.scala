package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.rule.Implicits.BaseRuleOps

import scala.language.higherKinds


class Var[U[_], T](data: U[T])(implicit r: ValueRule[U, T]) extends Node[U, T] {
  var gradient: U[T] = r.zeroAdd

  override def toString: String = s"Var[${ data }]"
  override def apply(): U[T] = data
  override def deriv(wrt: Node[U, T]): U[T] = {
    if (wrt == this) {
      r.zeroMul
    } else {
      r.zeroAdd
    }
  }

  override def propagate(g: U[T]): U[T] = { gradient += g * r.zeroMul; g }
}

object Var {
  def apply[U[_], T](data: U[T])(implicit r: ValueRule[U, T]): Var[U, T] = new Var[U, T](data)
  def apply[T](data: T)(implicit r: ValueRule[Scalar, T]): ScalarVar[T] = ScalarVar[T](data)
}
