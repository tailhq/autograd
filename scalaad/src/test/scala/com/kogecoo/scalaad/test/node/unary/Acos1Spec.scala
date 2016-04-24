package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{Acos1, V1, V2, Var1}
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdAcos1Spec extends Properties("Acos1") with Acos1Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V1): T1 = broadcast1(a.toT1, math.acos)

  override def deriv(a: T0): T0 = -1.0 / math.sqrt(1.0 - a * a)

  override def defaultMinValue = Some(-1e10)

  override def defaultMaxValue = Some( 1e10)

  override def domain = StdValueGen(
    Some(-1.0),
    Some(1.0),
    (x: Double) => x != -1.0 && x != 1.0
  )
}

trait Acos1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  def domain: Gen[T0]

  // exclude One1 node
  override def genArgV1ForSpecBase: Gen[Var1] = genV1(value = domain)

  override def genArgN1ForSpecBase: Gen[V1] = Gen.oneOf(
    genV1(value = domain),
    genConst1(value = domain),
    genHalf1(),
    genZero1()
  )

  override def genArgNV1ForSpecBase: Gen[V1] = Gen.oneOf(
    genConst1(value = domain),
    genHalf1(),
    genZero1()
  )

  override def genArgNV1_N1_ForSpecBase: Gen[(V1, V1)] = for {
    first  <- genArgNV1ForSpecBase
    second <- n1gen.genNode1(first.shape, genDefaultDomainValue)
  } yield (first, second)

  override def genArgNV1_N2_ForSpecBase: Gen[(V1, V2)] = for {
    first  <- genArgNV1ForSpecBase
    s2     =  genS2(first.shape._1)
    second <- n2gen.genNode2(s2, genDefaultDomainValue)
  } yield (first, second)

  override def genArgV1_N1_ForSpecBase: Gen[(Var1, V1)] = for {
    first  <- genArgV1ForSpecBase
    second <- n1gen.genNode1(first.shape, genDefaultDomainValue)
  } yield (first, second)

  override def genArgV1_N2_ForSpecBase: Gen[(Var1, V2)] = for {
    first  <- genArgV1ForSpecBase
    s2     =  genS2(first.shape._1)
    second <- n2gen.genNode2(s2, genDefaultDomainValue)
  } yield (first, second)


  override def op(a: V1): V1 = Acos1(a)

  override def op(argStr: String): String = s"acos($argStr)"

}


