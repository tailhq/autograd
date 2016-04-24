package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{V1, V2, Sqrt1}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdSqrt1Spec extends Properties("Sqrt1") with Sqrt1Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V1): T1 = broadcast1(a.toT1, math.sqrt)

  override def deriv(a: T0): T0 = 1 / (2 * math.sqrt(a))

  override def defaultMinValue = Some(0.0)

  override def defaultMaxValue = Some(100.0)

  override def defaultValueConstraint = (x: Double) => x != -0.0 && x != 0.0

}

trait Sqrt1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: V1): V1 = Sqrt1(a)

  override def op(argStr: String): String = s"sqrt($argStr)"

  override def genArgN1ForSpecBase: Gen[V1] = genNonzeroN1()

  override def genArgNV1ForSpecBase: Gen[V1] = genNonzeroNV1()

  override def genArgNV1_N1_ForSpecBase: Gen[(V1, V1)] = {
    for {
      first  <- genNonzeroNV1(genS1())
      second <- genN1(first.shape)
    } yield (first, second)
  }

  override def genArgNV1_N2_ForSpecBase: Gen[(V1, V2)] = {
    for {
      first  <- genNonzeroNV1()
      second <- genN2(genS2(first.shape._1))
    } yield (first, second)
  }

}


