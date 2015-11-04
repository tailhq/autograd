package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{Atan0, N0}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdAtan0Spec extends Properties("Atan0") with Atan0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N0): T0 = math.atan(a.toT0)

  override def deriv(a: T0): T0 = 1.0 / (1.0 + a * a)

  override def defaultMinValue = Some(-1e15)

  override def defaultMaxValue = Some( 1e15)

}


trait Atan0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  override def op(a: N0): N0 = Atan0(a)

  override def op(argStr: String): String = s"atan($argStr)"

}


