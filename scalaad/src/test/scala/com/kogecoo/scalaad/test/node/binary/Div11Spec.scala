package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdDiv11Spec extends Properties("Div11") with Div11Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V1, b: V1): T1 = a.toT1.elementwise(b.toT1, _ / _)

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  override def expectReverseLeftRight0(a: Var1, b: V0): T1 = {
    val x = a.toT1
    val y = b.toT0
    val l = broadcast1(x, div(y, _))
    val r = elementwise1(broadcast1(x, mul(_, -y)), elementwise1(x, x, mul), div)
    elementwise1(l, r, add)
  }

  override def expectReverseLeftRight1(a: Var1, b: V1): T1 = {
    val x = a.toT1
    val y = b.toT1
    val l = elementwise1(y, x, div)
    val r = elementwise1(x, y, (v, w) => div(-v * w, v * v))
    elementwise1(l, r, add)
  }

  override def expectReverseLeftRight2(a: Var1, b: V2): T2 = {
    val x = a.toT1
    val y = b.toT2
    val l = columnwise(y, x, div)
    val r = columnwise(y, x, (w, v) => div(-v * w, v * v))
    elementwise2(l, r, add)
  }

  override def leftDeriv(a: T0, b: T0): T0 = 1.0 / b

  override def rightDeriv(a: T0, b: T0): T0 = -a / (b * b)

  override def leftRightDeriv(a: T0): T0 = leftDeriv(a, a) + rightDeriv(a, a)

  override def defaultMinValue = Some(-1e10)

  override def defaultMaxValue = Some(1e10)

  override def denomDomain: Gen[T0] = StdValueGen(
    defaultMinValue,
    defaultMaxValue,
    (x: Double) => x != -0.0 && x != 0.0
  )
}

trait Div11Spec extends BinaryOp11SpecBase { self: Properties with SpecBackend =>

  def denomDomain: Gen[T0]

  override def op(a: V1, b: V1): V1 = Div11(a, b)

  override def op(a: String, b: String): String = s"$a / $b"


  override def genArgN1_ArgN1_ForSpecBase: Gen[(V1, V1)] = for {
    first  <- genN1()
    second <- genNonzeroN1(first.shape, denomDomain)
  } yield (first, second)

  override def genArgN1_ArgN1_N1_ForSpecBase: Gen[(V1, V1, V1)] = for {
    first  <- genN1()
    second <- genNonzeroN1(first.shape, denomDomain)
    third  <- genN1(first.shape)
  } yield (first, second, third  )

  override def genArgV1_ArgN1_ForSpecBase: Gen[(Var1, V1)] = for {
    first  <- genV1()
    second <- genNonzeroN1(first.shape, denomDomain)
  } yield (first, second)

  override def genArgN1_ArgV1_ForSpecBase: Gen[(V1, Var1)] = for {
    first  <- genN1()
    second <- genV1(first.shape, denomDomain)
  } yield (first, second)

  override def genArgV1_N1_ForSpecBase: Gen[(Var1, V1)] = for {
    first  <- genV1(value = denomDomain)
    second <- genN1(first.shape)
  } yield (first, second)

  override def genArgV1_N2_ForSpecBase: Gen[(Var1, V2)] = for {
    first  <- genV1(value = denomDomain)
    second <- genN2(genS2(first.shape))
  } yield (first, second)

  override def genArgNV1_ArgNV1_ForSpecBase: Gen[(V1, V1)] = for {
    first  <- genNV1()
    second <- genNonzeroNV1(first.shape, denomDomain)
  } yield (first, second)

  override def genArgNV1_ArgNV1_N1_ForSpecBase: Gen[(V1, V1, V1)] = for {
    first  <- genNV1()
    second <- genNonzeroNV1(first.shape, denomDomain)
    third  <- genN1(first.shape)
  } yield (first, second, third)

  override def genArgNV1_ArgNV1_N2_ForSpecBase: Gen[(V1, V1, V2)] = for {
    first  <- genNV1()
    second <- genNonzeroNV1(first.shape, denomDomain)
    third  <- genN2(genS2(first.shape))
  } yield (first, second, third)

  override def genArgNV1_ArgV1_ForSpecBase: Gen[(V1, Var1)] = for {
    first  <- genNV1()
    second <- genV1(first.shape, denomDomain)
  } yield (first, second)

  override def genArgNV1_ArgV1_N1_ForSpecBase: Gen[(V1, Var1, V1)] = for {
    first   <- genNV1()
    second  <- genV1(first.shape, denomDomain)
    third   <- genN1(first.shape)
  } yield (first, second, third)

  override def genArgNV1_ArgV1_N2_ForSpecBase: Gen[(V1, Var1, V2)] = for {
    first   <- genNV1()
    second  <- genV1(first.shape, denomDomain)
    third   <- genN2(genS2(first.shape))
  } yield (first, second, third)

  override def genArgV1_ArgNV1_ForSpecBase: Gen[(Var1, V1)] = for {
    first  <- genV1()
    second <- genNonzeroNV1(first.shape, denomDomain)
  } yield (first, second)

  override def genArgV1_ArgNV1_N1_ForSpecBase: Gen[(Var1, V1, V1)] = for {
    first   <- genV1()
    second  <- genNonzeroNV1(first.shape, denomDomain)
    third   <- genN1(first.shape)
  } yield (first, second, third)

  override def genArgV1_ArgNV1_N2_ForSpecBase: Gen[(Var1, V1, V2)] = for {
    first   <- genV1()
    second  <- genNonzeroNV1(first.shape, denomDomain)
    third   <- genN2(genS2(first.shape))
  } yield (first, second, third)

}
