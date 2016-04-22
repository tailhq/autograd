package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.N0
import com.kogecoo.scalaad.op.Cosh0
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdCosh0Spec extends Properties("Cosh0") with Cosh0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N0): T0 = math.cosh(a.toT0)

  override def deriv(a: T0): T0 = math.sinh(a)

  override def defaultMinValue = Some(-100.0)

  override def defaultMaxValue = Some( 100.0)

}

trait Cosh0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  def expectApplyOp(n: N0): T0

  def deriv(x: T0): T0

  override def op(a: N0): N0 = Cosh0(a)

  override def op(argStr: String): String = s"cosh($argStr)"

}


