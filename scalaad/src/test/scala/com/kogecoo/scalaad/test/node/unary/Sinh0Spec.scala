package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdSinhSpec extends Properties("Sinh0") with Sinh0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N0): T0 = math.sinh(a.toT0)

  override def deriv(a: T0): T0 = math.cosh(a)

  override def defaultMinValue = Some(-100.0)

  override def defaultMaxValue = Some(100.0)

}


trait Sinh0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  override def op(a: N0): N0 = Sinh0(a)

  override def op(argStr: String): String = s"sinh($argStr)"

}


