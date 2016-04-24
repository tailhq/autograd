package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{Acos2, V1, V2, Var2}
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdAcos2Spec extends Properties("Acos2") with Acos2Spec with StdSpecBackend {

  override def expectApplyOp(a: V2): T2 = broadcast2(a.toT2, math.acos)

  override def deriv(a: T0): T0 = -1.0 / math.sqrt(1.0 - a * a)

  override def defaultMinValue = Some(-1e10)

  override def defaultMaxValue = Some( 1e10)

  override def domain = StdValueGen(
    Some(-1.0),
    Some(1.0),
    (x: Double) => x != -1.0 && x != 1.0
  )

}

trait Acos2Spec extends UnaryOp2SpecBase { self: Properties with SpecBackend =>

  def domain: Gen[T0]

  override def op(a: String): String = s"acos($a)"

  override def op(a: V2): V2 = Acos2(a)

  // exclude One1 node
  override def genArgV2ForSpecBase: Gen[Var2] = genV2(value = domain)

  override def genArgN2ForSpecBase: Gen[V2] = Gen.oneOf(
    genV2(value = domain),
    genConst2(value = domain),
    genHalf2(),
    genZero2()
  )

  override def genArgNV2ForSpecBase: Gen[V2] = Gen.oneOf(
    genConst2(value = domain),
    genHalf2(),
    genZero2()
  )

  override def genArgNV2_N1_ForSpecBase: Gen[(V2, V1)] = {
    for {
      first  <- genArgNV2ForSpecBase
      s1     =  genS1(first.shape._1)
      second <- n1gen.genNode1(s1, genDefaultDomainValue)
    } yield (first, second)
  }

  override def genArgNV2_N2_ForSpecBase: Gen[(V2, V2)] = {
    for {
      first  <- genArgNV2ForSpecBase
      second <- n2gen.genNode2(first.shape, genDefaultDomainValue)
    } yield (first, second)
  }

  override def genArgV2_N1_ForSpecBase: Gen[(Var2, V1)] = {
    for {
      first  <- genArgV2ForSpecBase
      s1     =  genS1(first.shape._1)
      second <- n1gen.genNode1(s1, genDefaultDomainValue)
    } yield (first, second)
  }

  override def genArgV2_N2_ForSpecBase: Gen[(Var2, V2)] = {
    for {
      first  <- genArgV2ForSpecBase
      second <- n2gen.genNode2(first.shape, genDefaultDomainValue)
    } yield (first, second)
  }

}
