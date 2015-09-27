package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.rule.Implicits._


case class ScalarVar[T](data: T)(implicit r: ValueRule[Scalar, T]) extends Node[Scalar, T] {

  var gradient: Scalar[T] = r.zeroAdd

  override def toString: String = s"Var[${ data }]"
  override def apply(): Scalar[T] = r.wrap(data)
  override def deriv(wrt: Node[Scalar, T]): Scalar[T] = {
    if (wrt == this) {
      r.zeroMul
    } else {
      r.zeroAdd
    }
  }

  override def propagate(g: Scalar[T]): Scalar[T] = { gradient += g * r.zeroMul; g }
}

