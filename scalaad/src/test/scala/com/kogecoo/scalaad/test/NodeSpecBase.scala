package com.kogecoo.scalaad.test

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.helper.{Shape1Gen, Shape2Gen}
import com.kogecoo.scalaad.test.helper.impl.std._
import org.scalacheck.Gen
import shapeless.Nat.{_1, _2}


trait NodeSpecBase { self: SpecBackend =>

  def defaultMinValue: Option[T0] = None
  def defaultMaxValue: Option[T0] = None
  def defaultValueConstraint: T0 => Boolean = _ => true

  def defaultS1SizeConstraint: Int => Boolean = _ => true
  def defaultS1SizeMin: Int = 1
  def defaultS1SizeMax: Int = 10
  def defaultS1ShapeConstraint: Shape[_1] => Boolean = _ => true

  def defaultS2RowConstraint: Int => Boolean = _ => true
  def defaultS2RowMin: Int = 1
  def defaultS2RowMax: Int = 10
  def defaultS2ColConstraint: Int => Boolean = _ => true
  def defaultS2ColMin: Int = 1
  def defaultS2ColMax: Int = 10
  def defaultS2ShapeConstraint: Shape[_2] => Boolean = _ => true

  final def genDefaultDomainValue = genValue(defaultMinValue, defaultMaxValue, defaultValueConstraint)

  // alias
  private[this] def dom = genDefaultDomainValue

  final def genS1() = Shape1Gen(defaultS1SizeMin, defaultS1SizeMax, defaultS1SizeConstraint, defaultS1ShapeConstraint)
  final def genS1(size: Int) = Shape1Gen(size, defaultS1ShapeConstraint)
  final def genS2() = Shape2Gen(
    defaultS2RowMin,
    defaultS2RowMax,
    defaultS2RowConstraint,
    defaultS2ColMin,
    defaultS2ColMax,
    defaultS2ColConstraint,
    defaultS2ShapeConstraint
  )
  final def genS2(row: Int) = Shape2Gen(
    row,
    defaultS2ColMin,
    defaultS2ColMax,
    defaultS2ColConstraint,
    defaultS2ShapeConstraint
  )
  final def genS2(shape: Shape[_1]): Gen[Shape[_2]] = genS2(shape.at(0))

  // shorthands

  final def genConst0(                          value: Gen[T0] = dom) = n0gen.genConst0(       value)
  final def genConst1(shape: Gen[Shape[_1]] = genS1(), value: Gen[T0] = dom) = n1gen.genConst1(shape, value)
  final def genConst2(shape: Gen[Shape[_2]] = genS2(), value: Gen[T0] = dom) = n2gen.genConst2(shape, value)

  final def genHalf0()                         = n0gen.genHalf0()
  final def genHalf1(shape: Gen[Shape[_1]] = genS1()) = n1gen.genHalf1(shape)
  final def genHalf2(shape: Gen[Shape[_2]] = genS2()) = n2gen.genHalf2(shape)

  final def genOne0()                         = n0gen.genOne0()
  final def genOne1(shape: Gen[Shape[_1]] = genS1()) = n1gen.genOne1(shape)
  final def genOne2(shape: Gen[Shape[_2]] = genS2()) = n2gen.genOne2(shape)

  final def genZero0()                         = n0gen.genZero0()
  final def genZero1(shape: Gen[Shape[_1]] = genS1()) = n1gen.genZero1(shape)
  final def genZero2(shape: Gen[Shape[_2]] = genS2()) = n2gen.genZero2(shape)

  final def genV0(                          value: Gen[T0] = dom) = n0gen.genVar0(       value)
  final def genV1(shape: Gen[Shape[_1]] = genS1(), value: Gen[T0] = dom) = n1gen.genVar1(shape, value)
  final def genV2(shape: Gen[Shape[_2]] = genS2(), value: Gen[T0] = dom) = n2gen.genVar2(shape, value)

  final def genN0(                          value: Gen[T0] = dom) = n0gen.genNode0(       value)
  final def genN1(shape: Gen[Shape[_1]] = genS1(), value: Gen[T0] = dom) = n1gen.genNode1(shape, value)
  final def genN2(shape: Gen[Shape[_2]] = genS2(), value: Gen[T0] = dom) = n2gen.genNode2(shape, value)

  final def genNV0(                          value: Gen[T0] = dom) = n0gen.genNonVar0(       value)
  final def genNV1(shape: Gen[Shape[_1]] = genS1(), value: Gen[T0] = dom) = n1gen.genNonVar1(shape, value)
  final def genNV2(shape: Gen[Shape[_2]] = genS2(), value: Gen[T0] = dom) = n2gen.genNonVar2(shape, value)

  final def genNonzeroN0(                          value: Gen[T0] = dom) = n0gen.genNonzeroNode0(value)
  final def genNonzeroN1(shape: Gen[Shape[_1]] = genS1(), value: Gen[T0] = dom) = n1gen.genNonzeroNode1(shape, value)
  final def genNonzeroN2(shape: Gen[Shape[_2]] = genS2(), value: Gen[T0] = dom) = n2gen.genNonzeroNode2(shape, value)

  final def genNonzeroNV0(                          value: Gen[T0] = dom) = n0gen.genNonzeroNonVar0(value)
  final def genNonzeroNV1(shape: Gen[Shape[_1]] = genS1(), value: Gen[T0] = dom) = n1gen.genNonzeroNonVar1(shape, value)
  final def genNonzeroNV2(shape: Gen[Shape[_2]] = genS2(), value: Gen[T0] = dom) = n2gen.genNonzeroNonVar2(shape, value)

  final def genN1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(V1, V1)] = {
    for {
      first  <- n1gen.genNode1(genS1(), domain1)
      second <- n1gen.genNode1(first.shape, domain2)
    } yield (first, second)
  }

  final def genNV1_NV1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(V1, V1)] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n1gen.genNonVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genNV1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(V1, V1)] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n1gen.genNode1(first.shape, domain2)
    } yield (first, second)
  }

  final def genV1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(Var[_1], V1)] = {
    for {
      first  <- n1gen.genVar1(genS1(), domain1)
      second <- n1gen.genNode1(first.shape, domain2)
    } yield (first, second)
  }

  final def genV1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(Var[_1], V2)] = {
    for {
      first  <- n1gen.genVar1(genS1(), domain1)
      second <- n2gen.genNode2(genS2(first.shape.at(0)), domain2)
    } yield (first, second)
  }

  final def genNV1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(V1, V2)] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n2gen.genNode2(genS2(first.shape.at(0)), domain2)
    } yield (first, second)
  }

  final def genV1_NV1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(Var[_1], V1)] = {
    for {
      first  <- n1gen.genVar1(genS1(), domain1)
      second <- n1gen.genNonVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genN1_V1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(V1, Var[_1])] = {
    for {
      first  <- n1gen.genNode1(genS1(), domain1)
      second <- n1gen.genVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genNV1_V1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(V1, Var[_1])] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n1gen.genVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genNV2_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(V2, V1)] = {
    for {
      first  <- n2gen.genNonVar2(genS2(), domain1)
      s1     =  genS1(first.shape.at(0))
      second <- n1gen.genNode1(s1, domain2)
    } yield (first, second)
  }

  final def genNV2_N2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(V2, V2)] = {
    for {
      first  <- n2gen.genNonVar2(genS2(), domain1)
      second <- n2gen.genNode2(first.shape, domain2)
    } yield (first, second)
  }

  final def genV2_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(Var[_2], V1)] = {
    for {
      first  <- n2gen.genVar2(genS2(), domain1)
      s1     =  genS1(first.shape.at(0))
      second <- n1gen.genNode1(s1, domain2)
    } yield (first, second)
  }

  final def genV2_N2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(Var[_2], V2)] = {
    for {
      first  <- n2gen.genVar2(genS2(), domain1)
      second <- n2gen.genNode2(first.shape, domain2)
    } yield (first, second)
  }

  final def genV1_NV1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(Var[_1], V1, V1)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(first.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_V1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(V1, Var[_1], V1)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genV1_V1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(Var[_1], Var[_1], V1)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_NV1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(V1, V1, V1)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genN1_N1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(V1, V1, V1)] = {
    for {
      first   <- n1gen.genNode1(genS1(), domain1)
      second  <- n1gen.genNode1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_NV1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(V1, V1, V2)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape.at(0)), domain3)
    } yield (first, second, reverse)
  }

  final def genV1_NV1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(Var[_1], V1, V2)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape.at(0)), domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_V1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(V1, Var[_1], V2)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape.at(0)), domain3)
    } yield (first, second, reverse)
  }

  final def genV1_V1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(Var[_1], Var[_1], V2)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape.at(0)), domain3)
    } yield (first, second, reverse)
  }

}


