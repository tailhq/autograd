package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph.{N1, N2, Pow11, Var1}
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdPow11Spec extends Properties("Pow11") with Pow11Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N1, b: N1): T1 = elementwise1(a.toT1, b.toT1, math.pow)

  override def leftDeriv(a: T0, b: T0): T0 = b * math.pow(a, b - 1.0)

  override def rightDeriv(a: T0, b: T0): T0 = math.log(a) * math.pow(a, b)

  override def defaultMinValue = Some(-100.0)

  override def defaultMaxValue = Some(100.0)

  override def expDomain = StdValueGen(
    defaultMinValue,
    defaultMaxValue,
    (x: Double) => true
  )
  override def positiveBaseDomain = StdValueGen(
    Some(0.0),
    defaultMaxValue,
    (x: Double) => x != -0.0 && x != 0.0
  )
}


trait Pow11Spec extends BinaryOp11SpecBase { self: Properties with SpecBackend =>

  def expDomain: Gen[T0]

  def positiveBaseDomain: Gen[T0]

  override def op(a: N1, b: N1): N1 = Pow11(a, b)

  override def op(a: String, b: String): String = s"pow($a, $b)"

  override def genArgV1ForSpecBase: Gen[Var1] = genV1(value = positiveBaseDomain)

  override def genArgV1_N1_ForSpecBase: Gen[(Var1, N1)] = for {
    first  <- genV1(value = positiveBaseDomain)
    second <- genN1(first.shape)
  } yield (first, second)

  override def genArgV1_N2_ForSpecBase: Gen[(Var1, N2)] = for {
    first  <- genV1(value = positiveBaseDomain)
    second <- genN2(genS2(first.shape))
  } yield (first, second)

  override def genArgN1_ArgN1_ForSpecBase: Gen[(N1, N1)] = for {
    first  <- genNonzeroN1(value = positiveBaseDomain)
    second <- genN1(first.shape, expDomain)
  } yield (first, second)

  override def genArgN1_ArgN1_N1_ForSpecBase: Gen[(N1, N1, N1)] = for {
    first  <- genNonzeroN1(value = positiveBaseDomain)
    second <- genN1(first.shape, expDomain)
    third  <- genN1(first.shape)
  } yield (first, second, third)

  override def genArgV1_ArgN1_ForSpecBase: Gen[(Var1, N1)] = for {
    first   <- genV1(value = positiveBaseDomain)
    second  <- genN1(first.shape, expDomain)
  } yield (first, second)

  override def genArgV1_ArgNV1_ForSpecBase: Gen[(Var1, N1)] = for {
    first   <- genV1(value = positiveBaseDomain)
    second  <- genNV1(first.shape, expDomain)
  } yield (first, second)

  override def genArgN1_ArgV1_ForSpecBase: Gen[(N1, Var1)] = for {
    first  <- genNonzeroN1(value = positiveBaseDomain)
    second <- genV1(first.shape, expDomain)
  } yield (first, second)

  override def genArgNV1_ArgNV1_ForSpecBase: Gen[(N1, N1)] = for {
    first  <- genNonzeroNV1(value = positiveBaseDomain)
    second <- genNV1(first.shape, expDomain)
  } yield (first, second)

  override def genArgV1_ArgNV1_N1_ForSpecBase: Gen[(Var1, N1, N1)] = for {
    first  <- genV1(value = positiveBaseDomain)
    second <- genNV1(first.shape, expDomain)
    third  <- genN1(first.shape)
  } yield (first, second, third)

  override def genArgNV1_ArgNV1_N1_ForSpecBase: Gen[(N1, N1, N1)] = for {
    first  <- genNonzeroNV1(value = positiveBaseDomain)
    second <- genNV1(first.shape, expDomain)
    third  <- genN1(first.shape)
  } yield (first, second, third)

  override def genArgNV1_ArgNV1_N2_ForSpecBase: Gen[(N1, N1, N2)] = for {
    first  <- genNonzeroNV1(value = positiveBaseDomain)
    second <- genNV1(first.shape, expDomain)
    third  <- genN2(genS2(first.shape))
  } yield (first, second, third)

  override def genArgNV1_ArgV1_ForSpecBase: Gen[(N1, Var1)] = for {
    first  <- genNonzeroNV1(value = positiveBaseDomain)
    second <- genV1(first.shape, expDomain)
  } yield (first, second)

  override def genArgNV1_ArgV1_N1_ForSpecBase: Gen[(N1, Var1, N1)] = for {
    first   <- genNonzeroNV1(value = positiveBaseDomain)
    second  <- genV1(first.shape, expDomain)
    third   <- genN1(first.shape)
  } yield (first, second, third)

  override def genArgNV1_ArgV1_N2_ForSpecBase: Gen[(N1, Var1, N2)] = for {
    first   <- genNonzeroNV1(value = positiveBaseDomain)
    second  <- genV1(first.shape, expDomain)
    third   <- genN2(genS2(first.shape))
  } yield (first, second, third)

  override def genArgV1_ArgNV1_N2_ForSpecBase: Gen[(Var1, N1, N2)] = for {
    first   <- genV1(value = positiveBaseDomain)
    second  <- genNonzeroNV1(first.shape, expDomain)
    third   <- genN2(genS2(first.shape))
  } yield (first, second, third)

}
