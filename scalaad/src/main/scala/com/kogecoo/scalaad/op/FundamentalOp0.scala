package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.{Shape, Tensor}
import shapeless.Nat
import shapeless.Nat._2


case object ZeroOp extends NullaryOp

case object OneOp extends NullaryOp

case object HalfOp extends NullaryOp

case class ConstOp[N <: Nat](v: Tensor[N]) extends NullaryOp

case class EyeOp(shape: Shape[_2])