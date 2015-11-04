package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{Abs1, N1}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdAbs1Spec extends Properties("Abs1") with Abs1Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N1): T1 = broadcast1(a.toT1, math.abs)

  override def deriv(a: T0): T0 = if (a > 0.0) 1.0 else -1.0

}

trait Abs1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: N1): N1 = Abs1(a)

  override def op(argStr: String): String = s"abs($argStr)"

}


