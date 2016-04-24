package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph.V0
import com.kogecoo.scalaad.op.Max00
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdMax00Spec extends Properties("Max00") with Max00Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V0, b: V0): T0 = math.max(a.toT0, b.toT0)

  override def leftDeriv(a: T0, b: T0): T0 = if (a >= b) 1.0 else 0.0

  override def rightDeriv(a: T0, b: T0): T0 = if (a >= b) 0.0 else 1.0

  override def leftRightDeriv(a: T0): T0 = 1.0

  override def defaultMinValue = Some(-1e100)

  override def defaultMaxValue = Some( 1e100)

}


trait Max00Spec extends BinaryOp00SpecBase { self: Properties with SpecBackend =>

  override def op(a: V0, b: V0): V0 = Max00(a, b)

  override def op(a: String, b: String): String = s"max($a, $b)"

}

