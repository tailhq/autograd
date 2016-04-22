package com.kogecoo.scalaad.test.helper

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph._
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf
import shapeless.Nat._2


abstract class N2Gen[VG] {

  def genVar2(shapeGen: Gen[Shape[_2]], valueGen: Gen[VG]): Gen[Var2]

  def genConst2(shapeGen: Gen[Shape[_2]], valueGen: Gen[VG]): Gen[Const[_2]]

  final def genZero2(shapeGen: Gen[Shape[_2]]): Gen[Zero[_2]] = {
    for (s <- shapeGen; n <- Gen.const(Zero[_2](s))) yield n
  }

  final def genHalf2(shapeGen: Gen[Shape[_2]]): Gen[Half[_2]] = {
    for (s <- shapeGen; n <- Gen.const(Half[_2](s))) yield n
  }

  final def genOne2(shapeGen: Gen[Shape[_2]]): Gen[One[_2]] = {
    for (s <- shapeGen; n <- Gen.const(One[_2](s))) yield n
  }

  // composite

  final def genNode2(shapeGen: Gen[Shape[_2]], valueGen: Gen[VG]): Gen[V2] = {
    oneOf(
      genVar2(shapeGen, valueGen),
      genConst2(shapeGen, valueGen),
      genZero2(shapeGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

  final def genNonVar2(shapeGen: Gen[Shape[_2]], valueGen: Gen[VG]): Gen[V2] = {
    oneOf(
      genVar2(shapeGen, valueGen),
      genConst2(shapeGen, valueGen),
      genZero2(shapeGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

  final def genNonzeroNode2(shapeGen: Gen[Shape[_2]], valueGen: Gen[VG]): Gen[V2] = {
    oneOf(
      genConst2(shapeGen, valueGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

  final def genNonzeroNonVar2(shapeGen: Gen[Shape[_2]], valueGen: Gen[VG]): Gen[V2] = {
    oneOf(
      genConst2(shapeGen, valueGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

}
