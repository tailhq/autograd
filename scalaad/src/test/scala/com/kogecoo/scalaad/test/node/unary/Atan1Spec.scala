package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{Atan1, V1}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdAtan1Spec extends Properties("Atan1") with Atan1Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V1): T1 = broadcast1(a.toT1, math.atan)

  override def deriv(a: T0): T0 = 1.0 / (1.0 + a * a)

  override def defaultMinValue = Some(-1e15)

  override def defaultMaxValue = Some( 1e15)

}


trait Atan1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: V1): V1 = Atan1(a)

  override def op(argStr: String): String = s"atan($argStr)"

}
