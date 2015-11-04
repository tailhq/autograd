package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph.{N0, Sub00}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdSub00Spec extends Properties("Sub00") with Sub00Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N0, b: N0): T0 = a.toT0 - b.toT0

  override def leftDeriv(a: T0, b: T0): T0 = 1.0

  override def rightDeriv(a: T0, b: T0): T0 = -1.0

  override def leftRightDeriv(a: T0): T0 = 0.0

  override def defaultMinValue = Some(-1e100)

  override def defaultMaxValue = Some( 1e100)

}


trait Sub00Spec extends BinaryOp00SpecBase { self: Properties with SpecBackend =>

  override def op(a: N0, b: N0): N0 = Sub00(a, b)

  override def op(a: String, b: String): String = s"$a - $b"

}
