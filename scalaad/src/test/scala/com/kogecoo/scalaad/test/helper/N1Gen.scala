package com.kogecoo.scalaad.test.helper

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph._
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf
import shapeless.Nat._1

import scala.language.higherKinds


abstract class N1Gen[VG] {

  def genVar1(shapeGen: Gen[Shape[_1]], valueGen: Gen[VG]): Gen[Var[_1]]

  def genConst1(shapeGen: Gen[Shape[_1]], valueGen: Gen[VG]): Gen[Const[_1]]

  final def genZero1(shapeGen: Gen[Shape[_1]]): Gen[Zero[_1]] = {
    for (s <- shapeGen; n <- Gen.const(Zero[_1](s))) yield n
  }

  final def genHalf1(shapeGen: Gen[Shape[_1]]): Gen[Half[_1]] = {
    for (s <- shapeGen; n <- Gen.const(Half[_1](s))) yield n
  }

  final def genOne1(shapeGen: Gen[Shape[_1]]): Gen[One[_1]] = {
    for (s <- shapeGen; n <- Gen.const(One[_1](s))) yield n
  }

  // composite

  final def genNode1(shapeGen: Gen[Shape[_1]], valueGen: Gen[VG]): Gen[V1] = {
    oneOf(
      genVar1(shapeGen, valueGen),
      genConst1(shapeGen, valueGen),
      genZero1(shapeGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

  final def genNonVar1(shapeGen: Gen[Shape[_1]], valueGen: Gen[VG]): Gen[V1] = {
    oneOf(
      genConst1(shapeGen, valueGen),
      genZero1(shapeGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

  final def genNonzeroNode1(shapeGen: Gen[Shape[_1]], valueGen: Gen[VG]): Gen[V1] = {
    Gen.oneOf(
      genVar1(shapeGen, valueGen),
      genConst1(shapeGen, valueGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

  final def genNonzeroNonVar1(shapeGen: Gen[Shape[_1]], valueGen: Gen[VG]): Gen[V1] = {
    Gen.oneOf(
      genConst1(shapeGen, valueGen),
      genHalf1(shapeGen),
      genOne1(shapeGen)
    )
  }

}

