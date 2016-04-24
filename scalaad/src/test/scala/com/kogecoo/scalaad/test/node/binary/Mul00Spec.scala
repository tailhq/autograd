package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph.{Mul00, V0}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdMul00Spec extends Properties("Mul00") with Mul00Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V0, b: V0): T0 = a.toT0 * b.toT0

  override def leftDeriv(a: T0, b: T0): T0 = b

  override def rightDeriv(a: T0, b: T0): T0 = a

  override def leftRightDeriv(a: T0): T0 = 2 * a

  override def defaultMinValue = Some(-1e100)

  override def defaultMaxValue = Some( 1e100)

}


trait Mul00Spec extends BinaryOp00SpecBase { self: Properties with SpecBackend =>

  override def op(a: V0, b: V0): V0 = Mul00(a, b)

  override def op(a: String, b: String): String = s"$a * $b"

}
