package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdDiv00Spec extends Properties("Div00") with Div00Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V0, b: V0): T0 = a.toT0 / b.toT0

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  override def expectReverseLeftRight0(a: Var0, b: V0): T0 = {
    val x = a.toT0
    val y = b.toT0
    val l = y / x
    val r = -(x * y) / (x * x)
    l + r
  }

  override def expectReverseLeftRight1(a: Var0, b: V1): T1 = {
    val x = a.toT0
    val y = b.toT1
    val l = broadcast1(y, div(_, x))
    val r = broadcast1(y, z => div(-x * z, x * x))
    elementwise1(l, r, add)
  }

  override def expectReverseLeftRight2(a: Var0, b: V2): T2 = {
    val x = a.toT0
    val y = b.toT2
    val l = broadcast2(y, div(_, x))
    val r = broadcast2(y, z => div(-x * z, x * x))
    elementwise2(l, r, add)
  }

  override def leftDeriv(a: T0, b: T0): T0 = 1.0 / b

  override def rightDeriv(a: T0, b: T0): T0 = -a / (b * b)

  override def leftRightDeriv(a: T0): T0 = leftDeriv(a, a) + rightDeriv(a, a)

  override def defaultMinValue = Some(-1e10)

  override def defaultMaxValue = Some( 1e10)

  override def denomDomain: Gen[T0] = StdValueGen(
    defaultMinValue,
    defaultMaxValue,
    (x: Double) => x != -0.0 && x != 0.0
  )


}


trait Div00Spec extends BinaryOp00SpecBase { self: Properties with SpecBackend =>

  def denomDomain: Gen[T0]

  override def op(a: V0, b: V0): V0 = Div00(a, b)

  override def op(a: String, b: String): String = s"$a / $b"

  override def genRightArgN0ForSpecBase: Gen[V0] = genNonzeroN0(denomDomain)

  override def genRightArgNV0ForSpecBase: Gen[V0] = genNonzeroNV0(denomDomain)

  override def genRightArgV0ForSpecBase: Gen[Var0] = genV0(denomDomain)

  override def genLeftRightArgV0ForSpecBase: Gen[Var0] = genV0(denomDomain)
}

