package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.V0
import com.kogecoo.scalaad.op.Tan0
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdTan0Spec extends Properties("Tan0") with Tan0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V0): T0 = math.tan(a.toT0)

  override def deriv(a: T0): T0 = 1 + math.tan(a) * math.tan(a)

  override def defaultMinValue = Some(-1e10)

  override def defaultMaxValue = Some(1e10)

}


trait Tan0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  override def op(a: V0): V0 = Tan0(a)

  override def op(argStr: String): String = s"tan($argStr)"

}


