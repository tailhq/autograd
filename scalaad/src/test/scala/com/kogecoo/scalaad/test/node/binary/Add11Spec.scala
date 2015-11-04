package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdAdd11Spec extends Properties("Add11") with Add11Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N1, b: N1): T1 = elementwise1(a.toT1, b.toT1, _ + _)

  override def leftDeriv(a: T0, b: T0): T0 = 1.0

  override def rightDeriv(a: T0, b: T0): T0 = 1.0

  override def leftRightDeriv(a: T0): T0 = 2.0

  override def defaultMinValue = Some(-1e100)

  override def defaultMaxValue = Some( 1e100)

}


trait Add11Spec extends BinaryOp11SpecBase { self: Properties with SpecBackend =>

  override def op(a: N1, b: N1): N1 = Add11(a, b)

  override def op(a: String, b: String): String = s"$a + $b"

}

