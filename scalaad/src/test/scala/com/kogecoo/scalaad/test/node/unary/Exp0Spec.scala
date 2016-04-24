package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.V0
import com.kogecoo.scalaad.op.Exp0
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdExp0Spec extends Properties("Exp0") with Exp0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V0): T0 = math.exp(a.toT0)

  override def deriv(a: T0): T0 = math.exp(a)

  override def defaultMinValue = Some(-100.0)

  override def defaultMaxValue = Some(100.0)

}

trait Exp0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  override def op(a: V0): V0 = Exp0(a)

  override def op(argStr: String): String = s"exp($argStr)"

}
