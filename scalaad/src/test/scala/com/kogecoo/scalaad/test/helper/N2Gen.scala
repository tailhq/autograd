package scalaad.test.helper

import scalaad.Shape
import scalaad.graph._
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf


abstract class N2Gen[VG] {

  def genVar2(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[Var]

  def genConst2(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[Const]

  final def genZero2(shapeGen: Gen[Shape]): Gen[Zero] = {
    for (s <- shapeGen; n <- Gen.const(Zero(s))) yield n
  }

  final def genHalf2(shapeGen: Gen[Shape]): Gen[Half] = {
    for (s <- shapeGen; n <- Gen.const(Half(s))) yield n
  }

  final def genOne2(shapeGen: Gen[Shape]): Gen[One] = {
    for (s <- shapeGen; n <- Gen.const(One(s))) yield n
  }

  // composite

  final def genNode2(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[Expr] = {
    oneOf(
      genVar2(shapeGen, valueGen),
      genConst2(shapeGen, valueGen),
      genZero2(shapeGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

  final def genNonVar2(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[Expr] = {
    oneOf(
      genVar2(shapeGen, valueGen),
      genConst2(shapeGen, valueGen),
      genZero2(shapeGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

  final def genNonzeroNode2(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[Expr] = {
    oneOf(
      genConst2(shapeGen, valueGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

  final def genNonzeroNonVar2(shapeGen: Gen[Shape], valueGen: Gen[VG]): Gen[Expr] = {
    oneOf(
      genConst2(shapeGen, valueGen),
      genHalf2(shapeGen),
      genOne2(shapeGen)
    )
  }

}
