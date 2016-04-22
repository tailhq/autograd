package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.N0
import com.kogecoo.scalaad.op.Sqrt0
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdSqrt0Spec extends Properties("Sqrt0") with Sqrt0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N0): T0 = math.sqrt(a.toT0)

  override def deriv(a: T0): T0 = 1.0 / (2.0 * math.sqrt(a))

  override def defaultMinValue = Some(0.0)

  override def defaultMaxValue = Some(100.0)

  override def defaultValueConstraint = (x: Double) => x != -0.0 && x != 0.0

}


trait Sqrt0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  override def op(a: N0): N0 = Sqrt0(a)

  override def op(argStr: String): String = s"sqrt($argStr)"

  override def genArgN0ForSpecBase: Gen[N0] = genNonzeroN0()

  override def genArgNV0ForSpecBase: Gen[N0] = genNonzeroNV0()

}


