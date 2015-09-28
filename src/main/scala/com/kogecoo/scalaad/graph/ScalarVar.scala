package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.rule.Implicits._


case class ScalarVar[T](data: T)(implicit r: ValueRule[Scalar, T]) extends Node[Scalar, T] {

  var gradient: Value[Scalar, T] = r.zeroAdd

  override def toString: String = s"Var[${ data }]"
  override def apply(): Value[Scalar, T] = r.wrap(data)
  override def deriv(wrt: Node[Scalar, T]): Value[Scalar, T] = {
    if (wrt == this) {
      r.zeroMul
    } else {
      r.zeroAdd
    }
  }

  override def propagate(g: Value[Scalar, T]): Value[Scalar, T] = { gradient += g * r.zeroMul; g }
}

