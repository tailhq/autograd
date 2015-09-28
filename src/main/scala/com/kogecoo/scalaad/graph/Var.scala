package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.rule.Implicits._

import scala.language.higherKinds


class Var[U[_], T](data: U[T])(implicit r: ValueRule[U, T]) extends Node[U, T] {
  var gradient: Value[U, T] = r.zeroAdd

  override def toString: String = s"Var[${ data }]"
  override def apply(): Value[U, T] = ContainerValue[U, T](data)
  override def deriv(wrt: Node[U, T]): Value[U, T] = {
    if (wrt == this) {
      r.zeroMul
    } else {
      r.zeroAdd
    }
  }

  override def propagate(g: Value[U, T]): Value[U, T] = { gradient += g * r.zeroMul; g }
}

object Var {
  def apply[U[_], T](data: U[T])(implicit r: ValueRule[U, T]): Var[U, T] = new Var[U, T](data)
  def apply[T](data: T)(implicit r: ValueRule[Scalar, T]): ScalarVar[T] = ScalarVar[T](data)
}
