package com.kogecoo.scalaad.test

import com.kogecoo.scalaad.Shape2
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.helper.impl.std._
import org.scalacheck.Gen


trait NodeSpecBase { self: SpecBackend =>

  private[this] final def checkSquare(shape: S2): Unit = {
    if (shape._1 != shape._2) {
      throw new Exception(s"row/col size of $shape requires to be same size")
    }
  }

  final def zero1(s1: S1): T1 = const1(zero0, s1)
  final def zero1(l1: N1): T1 = zero1(l1.shape)

  final def zero2(s2: S2): T2                 = const2(zero0, s2)
  final def zero2(l2: N2): T2                 = zero2(l2.shape)
  final def zero2(rowRef: N1, colRef: N1): T2 = zero2(Shape2(rowRef.shape._1, colRef.shape._1))

  final def one1(s1: S1): T1 = const1(one0, s1)
  final def one1(l1: N1): T1 = one1(l1.shape)

  final def one2(s2: S2): T2 = const2(one0, s2)
  final def one2(l2: N2): T2 = one2(l2.shape)

  final def const1(v: T0, l1: N1): T1 = const1(v, l1.shape)
  final def const2(v: T0, l2: N2): T2 = const2(v, l2.shape)

  final def eye(shape: S2): T2              = { checkSquare(shape); diag(one0, shape._1) }
  final def eye(rowRef: N1, colRef: N1): T2 = eye(Shape2(rowRef, colRef))
  final def eye(l1: N1): T2                 = diag(one0, l1.shape._1)

  def defaultMinValue: Option[T0] = None
  def defaultMaxValue: Option[T0] = None
  def defaultValueConstraint: T0 => Boolean = _ => true

  def defaultS1SizeConstraint: Int => Boolean = _ => true
  def defaultS1SizeMin: Int = 1
  def defaultS1SizeMax: Int = 10
  def defaultS1ShapeConstraint: S1 => Boolean = _ => true

  def defaultS2RowConstraint: Int => Boolean = _ => true
  def defaultS2RowMin: Int = 1
  def defaultS2RowMax: Int = 10
  def defaultS2ColConstraint: Int => Boolean = _ => true
  def defaultS2ColMin: Int = 1
  def defaultS2ColMax: Int = 10
  def defaultS2ShapeConstraint: S2 => Boolean = _ => true

  final def genDefaultDomainValue = genValue(defaultMinValue, defaultMaxValue, defaultValueConstraint)

  // alias
  private[this] def dom = genDefaultDomainValue

  final def genS1() = S1Gen(defaultS1SizeConstraint, defaultS1ShapeConstraint, defaultS1SizeMin, defaultS1SizeMax)
  final def genS1(size: Int) = S1Gen(size, defaultS1ShapeConstraint)
  final def genS2() = S2Gen(
    defaultS2RowConstraint,
    defaultS2ColConstraint,
    defaultS2ShapeConstraint,
    defaultS2RowMin,
    defaultS2RowMax,
    defaultS2ColMin,
    defaultS2ColMax
  )
  final def genS2(row: Int) = S2Gen(
    row,
    defaultS2ColConstraint,
    defaultS2ShapeConstraint,
    defaultS2ColMin,
    defaultS2ColMax
  )
  final def genS2(shape: S1): Gen[S2] = genS2(shape._1)

  // shorthands

  final def genConst0(                          value: Gen[T0] = dom) = n0gen.genConst0(       value)
  final def genConst1(shape: Gen[S1] = genS1(), value: Gen[T0] = dom) = n1gen.genConst1(shape, value)
  final def genConst2(shape: Gen[S2] = genS2(), value: Gen[T0] = dom) = n2gen.genConst2(shape, value)

  final def genHalf0()                         = n0gen.genHalf0()
  final def genHalf1(shape: Gen[S1] = genS1()) = n1gen.genHalf1(shape)
  final def genHalf2(shape: Gen[S2] = genS2()) = n2gen.genHalf2(shape)

  final def genOne0()                         = n0gen.genOne0()
  final def genOne1(shape: Gen[S1] = genS1()) = n1gen.genOne1(shape)
  final def genOne2(shape: Gen[S2] = genS2()) = n2gen.genOne2(shape)

  final def genZero0()                         = n0gen.genZero0()
  final def genZero1(shape: Gen[S1] = genS1()) = n1gen.genZero1(shape)
  final def genZero2(shape: Gen[S2] = genS2()) = n2gen.genZero2(shape)

  final def genV0(                          value: Gen[T0] = dom) = n0gen.genVar0(       value)
  final def genV1(shape: Gen[S1] = genS1(), value: Gen[T0] = dom) = n1gen.genVar1(shape, value)
  final def genV2(shape: Gen[S2] = genS2(), value: Gen[T0] = dom) = n2gen.genVar2(shape, value)

  final def genN0(                          value: Gen[T0] = dom) = n0gen.genNode0(       value)
  final def genN1(shape: Gen[S1] = genS1(), value: Gen[T0] = dom) = n1gen.genNode1(shape, value)
  final def genN2(shape: Gen[S2] = genS2(), value: Gen[T0] = dom) = n2gen.genNode2(shape, value)

  final def genNV0(                          value: Gen[T0] = dom) = n0gen.genNonVar0(       value)
  final def genNV1(shape: Gen[S1] = genS1(), value: Gen[T0] = dom) = n1gen.genNonVar1(shape, value)
  final def genNV2(shape: Gen[S2] = genS2(), value: Gen[T0] = dom) = n2gen.genNonVar2(shape, value)

  final def genNonzeroN0(                          value: Gen[T0] = dom) = n0gen.genNonzeroNode0(value)
  final def genNonzeroN1(shape: Gen[S1] = genS1(), value: Gen[T0] = dom) = n1gen.genNonzeroNode1(shape, value)
  final def genNonzeroN2(shape: Gen[S2] = genS2(), value: Gen[T0] = dom) = n2gen.genNonzeroNode2(shape, value)

  final def genNonzeroNV0(                          value: Gen[T0] = dom) = n0gen.genNonzeroNonVar0(value)
  final def genNonzeroNV1(shape: Gen[S1] = genS1(), value: Gen[T0] = dom) = n1gen.genNonzeroNonVar1(shape, value)
  final def genNonzeroNV2(shape: Gen[S2] = genS2(), value: Gen[T0] = dom) = n2gen.genNonzeroNonVar2(shape, value)

  final def genN1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(N1, N1)] = {
    for {
      first  <- n1gen.genNode1(genS1(), domain1)
      second <- n1gen.genNode1(first.shape, domain2)
    } yield (first, second)
  }

  final def genNV1_NV1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(N1, N1)] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n1gen.genNonVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genNV1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(N1, N1)] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n1gen.genNode1(first.shape, domain2)
    } yield (first, second)
  }

  final def genV1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(Var1, N1)] = {
    for {
      first  <- n1gen.genVar1(genS1(), domain1)
      second <- n1gen.genNode1(first.shape, domain2)
    } yield (first, second)
  }

  final def genV1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(Var1, N2)] = {
    for {
      first  <- n1gen.genVar1(genS1(), domain1)
      second <- n2gen.genNode2(genS2(first.shape._1), domain2)
    } yield (first, second)
  }

  final def genNV1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(N1, N2)] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n2gen.genNode2(genS2(first.shape._1), domain2)
    } yield (first, second)
  }

  final def genV1_NV1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(Var1, N1)] = {
    for {
      first  <- n1gen.genVar1(genS1(), domain1)
      second <- n1gen.genNonVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genN1_V1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(N1, Var1)] = {
    for {
      first  <- n1gen.genNode1(genS1(), domain1)
      second <- n1gen.genVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genNV1_V1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(N1, Var1)] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n1gen.genVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genNV2_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(N2, N1)] = {
    for {
      first  <- n2gen.genNonVar2(genS2(), domain1)
      s1     =  genS1(first.shape._1)
      second <- n1gen.genNode1(s1, domain2)
    } yield (first, second)
  }

  final def genNV2_N2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(N2, N2)] = {
    for {
      first  <- n2gen.genNonVar2(genS2(), domain1)
      second <- n2gen.genNode2(first.shape, domain2)
    } yield (first, second)
  }

  final def genV2_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(Var2, N1)] = {
    for {
      first  <- n2gen.genVar2(genS2(), domain1)
      s1     =  genS1(first.shape._1)
      second <- n1gen.genNode1(s1, domain2)
    } yield (first, second)
  }

  final def genV2_N2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom): Gen[(Var2, N2)] = {
    for {
      first  <- n2gen.genVar2(genS2(), domain1)
      second <- n2gen.genNode2(first.shape, domain2)
    } yield (first, second)
  }

  final def genV1_NV1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(Var1, N1, N1)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(first.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_V1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(N1, Var1, N1)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genV1_V1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(Var1, Var1, N1)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_NV1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(N1, N1, N1)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genN1_N1_N1(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(N1, N1, N1)] = {
    for {
      first   <- n1gen.genNode1(genS1(), domain1)
      second  <- n1gen.genNode1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_NV1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(N1, N1, N2)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape._1), domain3)
    } yield (first, second, reverse)
  }

  final def genV1_NV1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(Var1, N1, N2)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape._1), domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_V1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(N1, Var1, N2)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape._1), domain3)
    } yield (first, second, reverse)
  }

  final def genV1_V1_RowEquivN2(domain1: Gen[T0] = dom, domain2: Gen[T0] = dom, domain3: Gen[T0] = dom): Gen[(Var1, Var1, N2)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape._1), domain3)
    } yield (first, second, reverse)
  }

}


