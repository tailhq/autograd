package com.kogecoo.scalaad.test.helper

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph._
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

import scala.language.higherKinds


abstract class N1Gen[VG] {

  def genVar1(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[Var]

  def genConst1(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[Const]

  final def genZero1(shapeGen: Gen[Shape]): Gen[Zero] = {
    for (s <- shapeGen; n <- Gen.const(Zero(s))) yield n
  }

  final def genHalf1(shapeGen: Gen[Shape]): Gen[Half] = {
    for (s <- shapeGen; n <- Gen.const(Half(s))) yield n
  }

  final def genOne1(shapeGen: Gen[Shape]): Gen[One] = {
    for (s <- shapeGen; n <- Gen.const(One(s))) yield n
  }

  // composite

  final def genNode1(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[V] = {
    oneOf(
      genVar1(shapeGen, valueGen),
      genConst1(shapeGen, valueGen),
      genZero1(shapeGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

  final def genNonVar1(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[V] = {
    oneOf(
      genConst1(shapeGen, valueGen),
      genZero1(shapeGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

  final def genNonzeroNode1(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[V] = {
    Gen.oneOf(
      genVar1(shapeGen, valueGen),
      genConst1(shapeGen, valueGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

  final def genNonzeroNonVar1(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[V] = {
    Gen.oneOf(
      genConst1(shapeGen, valueGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

}

