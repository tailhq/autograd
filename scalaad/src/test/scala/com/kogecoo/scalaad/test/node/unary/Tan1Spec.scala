package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{V1, Tan1}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdTan1Spec extends Properties("Tan1") with Tan1Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V1): T1 = broadcast1(a.toT1, math.tan)

  override def deriv(a: T0): T0 = 1 + math.tan(a) * math.tan(a)

  override def defaultMinValue = Some(-1e10)

  override def defaultMaxValue = Some(1e10)

}

trait Tan1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: V1): V1 = Tan1(a)

  override def op(argStr: String): String = s"tan($argStr)"

}
