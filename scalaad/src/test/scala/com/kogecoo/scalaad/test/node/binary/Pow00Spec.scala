package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph.{V0, Var0}
import com.kogecoo.scalaad.op.Pow00
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdPow00Spec extends Properties("Pow00") with Pow00Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V0, b: V0): T0 = math.pow(a.toT0, b.toT0)

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


trait Pow00Spec extends BinaryOp00SpecBase { self: Properties with SpecBackend =>

  def expDomain: Gen[T0]

  def positiveBaseDomain: Gen[T0]

  override def op(a: V0, b: V0): V0 = Pow00(a, b)

  override def op(a: String, b: String): String = s"pow($a, $b)"

  override def genLeftArgN0ForSpecBase: Gen[V0] = genNonzeroN0(positiveBaseDomain)

  override def genLeftArgNV0ForSpecBase: Gen[V0] = genNonzeroNV0(positiveBaseDomain)

  override def genLeftArgV0ForSpecBase: Gen[Var0] = genV0(positiveBaseDomain)

  override def genRightArgN0ForSpecBase: Gen[V0] = genNonzeroN0(expDomain)

  override def genRightArgNV0ForSpecBase: Gen[V0] = genNonzeroNV0(expDomain)

  override def genRightArgV0ForSpecBase: Gen[Var0] = genV0(expDomain)

  override def genLeftRightArgV0ForSpecBase: Gen[Var0] = genV0(positiveBaseDomain)
}

