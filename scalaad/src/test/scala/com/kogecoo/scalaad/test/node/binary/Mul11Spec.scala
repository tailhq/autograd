package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph.{Mul11, V1}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdMul11Spec extends Properties("Mul11") with Mul11Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V1, b: V1): T1 = elementwise1(a.toT1, b.toT1, _ * _)

  override def leftDeriv(a: T0, b: T0): T0 = b

  override def rightDeriv(a: T0, b: T0): T0 = a

  override def leftRightDeriv(a: T0): T0 = 2 * a

  override def defaultMinValue = Some(-1e100)

  override def defaultMaxValue = Some( 1e100)

}


trait Mul11Spec extends BinaryOp11SpecBase { self: Properties with SpecBackend =>

  override def op(a: V1, b: V1): V1 = Mul11(a, b)

  override def op(a: String, b: String): String = s"$a * $b"

}

