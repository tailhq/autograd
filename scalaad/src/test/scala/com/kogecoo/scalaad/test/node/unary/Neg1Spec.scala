package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{V1, Neg1}
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdNeg1Spec extends Properties("Neg1") with Neg1Spec with StdSpecBackend {

  override def expectApplyOp(a: V1): T1 = broadcast1(a.toT1, -_)

  override def deriv(a: T0): T0 = -1.0

}

trait Neg1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: V1): V1 = Neg1(a)

  override def op(argStr: String): String = s"-$argStr"

}


