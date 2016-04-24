package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{V0, Neg0}
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdNeg0Spec extends Properties("Neg0") with Neg0Spec with StdSpecBackend {

  override def expectApplyOp(a: V0): T0 = -a.toT0

  override def deriv(a: T0): T0 =  -1.0

}

trait Neg0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  override def op(a: V0): V0 = Neg0(a)

  override def op(argStr: String): String = s"-$argStr"

}

