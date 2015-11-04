package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{N1, Sin1}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdSin1Spec extends Properties("Sin1") with Sin1Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N1): T1 = broadcast1(a.toT1, math.sin)

  override def deriv(a: T0): T0 = math.cos(a)

}

trait Sin1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: N1): N1 = Sin1(a)

  override def op(argStr: String): String = s"sin($argStr)"

}
