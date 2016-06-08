package com.kogecoo.scalaad.test.helper

import com.kogecoo.scalaad.S2
import com.kogecoo.scalaad.graph._
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf


abstract class N2Gen[VG] {

  def genVar2(shapeGen: Gen[S2], valueGen: Gen[VG]): Gen[Var2]

  def genConst2(shapeGen: Gen[S2], valueGen: Gen[VG]): Gen[Const2]

  final def genZero2(shapeGen: Gen[S2]): Gen[Zero2] = {
    for (s <- shapeGen; n <- Gen.const(Zero2(s))) yield n
  }

  final def genHalf2(shapeGen: Gen[S2]): Gen[Half2] = {
    for (s <- shapeGen; n <- Gen.const(Half2(s))) yield n
  }

  final def genOne2(shapeGen: Gen[S2]): Gen[One2] = {
    for (s <- shapeGen; n <- Gen.const(One2(s))) yield n
  }

  // composite

  final def genNode2(shapeGen: Gen[S2], valueGen: Gen[VG]): Gen[V2] = {
    oneOf(
      genVar2(shapeGen, valueGen),
      genConst2(shapeGen, valueGen),
      genZero2(shapeGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

  final def genNonVar2(shapeGen: Gen[S2], valueGen: Gen[VG]): Gen[V2] = {
    oneOf(
      genVar2(shapeGen, valueGen),
      genConst2(shapeGen, valueGen),
      genZero2(shapeGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

  final def genNonzeroNode2(shapeGen: Gen[S2], valueGen: Gen[VG]): Gen[V2] = {
    oneOf(
      genConst2(shapeGen, valueGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

  final def genNonzeroNonVar2(shapeGen: Gen[S2], valueGen: Gen[VG]): Gen[V2] = {
    oneOf(
      genConst2(shapeGen, valueGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

}
