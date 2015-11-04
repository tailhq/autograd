package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{N1, Sinh1}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdSinh1Spec extends Properties("Sinh1") with Sinh1Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N1): T1 = broadcast1(a.toT1, math.sinh)

  override def deriv(a: T0): T0 = math.cosh(a)

  override def defaultMinValue = Some(-100.0)

  override def defaultMaxValue = Some( 100.0)

}

trait Sinh1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: N1): N1 = Sinh1(a)

  override def op(argStr: String): String = s"sinh($argStr)"

}
