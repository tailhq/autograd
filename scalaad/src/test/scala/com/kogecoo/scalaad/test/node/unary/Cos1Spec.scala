package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{Cos1, N1}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdCos1Spec extends Properties("Cos1") with Cos1Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N1): T1 = broadcast1(a.toT1, math.cos)

  override def deriv(a: T0): T0 = -math.sin(a)

}

trait Cos1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: N1): N1 = Cos1(a)

  override def op(argStr: String): String = s"cos($argStr)"

}
