package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{Ln1, N1, N2, Var1}
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdLn1Spec extends Properties("Ln1") with Ln1Spec with StdSpecBackend {

  override def expectApplyOp(a: N1): T1 = broadcast1(a.toT1, math.log)

  override def deriv(a: T0): T0 = 1 / a

  override def defaultMinValue = Some(0.0)

  override def defaultMaxValue = Some(100.0)

  override def defaultValueConstraint = (x: Double) => x != -0.0 && x != 0.0

}


trait Ln1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: N1): N1 = Ln1(a)

  override def op(argStr: String): String = s"ln($argStr)"

  override def genArgN1ForSpecBase: Gen[N1] = genNonzeroN1()

  override def genArgNV1ForSpecBase: Gen[N1] = genNonzeroNV1()

  override def genArgNV1_N1_ForSpecBase: Gen[(N1, N1)] = {
    for {
      first  <- genNonzeroNV1(genS1())
      second <- genN1(first.shape)
    } yield (first, second)
  }

  override def genArgNV1_N2_ForSpecBase: Gen[(N1, N2)] = {
    for {
      first  <- genNonzeroNV1()
      second <- genN2(genS2(first.shape._1))
    } yield (first, second)
  }

}


