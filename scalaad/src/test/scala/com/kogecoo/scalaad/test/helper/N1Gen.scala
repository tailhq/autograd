package com.kogecoo.scalaad.test.helper

import com.kogecoo.scalaad.S1
import com.kogecoo.scalaad.graph._
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

import scala.language.higherKinds


abstract class N1Gen[VG] {

  def genVar1(shapeGen: Gen[S1], valueGen: Gen[VG]): Gen[Var1]

  def genConst1(shapeGen: Gen[S1], valueGen: Gen[VG]): Gen[Const1]

  final def genZero1(shapeGen: Gen[S1]): Gen[Zero1] = {
    for (s <- shapeGen; n <- Gen.const(Zero1(s))) yield n
  }

  final def genHalf1(shapeGen: Gen[S1]): Gen[Half1] = {
    for (s <- shapeGen; n <- Gen.const(Half1(s))) yield n
  }

  final def genOne1(shapeGen: Gen[S1]): Gen[One1] = {
    for (s <- shapeGen; n <- Gen.const(One1(s))) yield n
  }

  // composite

  final def genNode1(shapeGen: Gen[S1], valueGen: Gen[VG]): Gen[VE1] = {
    oneOf(
      genVar1(shapeGen, valueGen),
      genConst1(shapeGen, valueGen),
      genZero1(shapeGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

  final def genNonVar1(shapeGen: Gen[S1], valueGen: Gen[VG]): Gen[VE1] = {
    oneOf(
      genConst1(shapeGen, valueGen),
      genZero1(shapeGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

  final def genNonzeroNode1(shapeGen: Gen[S1], valueGen: Gen[VG]): Gen[VE1] = {
    Gen.oneOf(
      genVar1(shapeGen, valueGen),
      genConst1(shapeGen, valueGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

  final def genNonzeroNonVar1(shapeGen: Gen[S1], valueGen: Gen[VG]): Gen[VE1] = {
    Gen.oneOf(
      genConst1(shapeGen, valueGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

}

