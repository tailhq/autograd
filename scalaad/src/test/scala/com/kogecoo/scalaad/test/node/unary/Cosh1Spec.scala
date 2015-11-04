package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{Cosh1, N1}
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdCosh1Spec extends Properties("Cosh1") with Cosh1Spec with StdSpecBackend {

  override def expectApplyOp(a: N1): T1 = broadcast1(a.toT1, math.cosh)

  override def deriv(a: T0): T0 = math.sinh(a)

  override def defaultMinValue = Some(-100.0)

  override def defaultMaxValue = Some( 100.0)

}


trait Cosh1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: N1): N1 = Cosh1(a)

  override def op(argStr: String): String = s"cosh($argStr)"

}


