package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{N1, Tanh1}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdTanh1Spec extends Properties("Tanh1") with Tanh1Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N1): T1 = broadcast1(a.toT1, math.tanh)

  override def deriv(a: T0): T0 = 1 - math.tanh(a) * math.tanh(a)

}

trait Tanh1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: N1): N1 = Tanh1(a)

  override def op(argStr: String): String = s"tanh($argStr)"

}
