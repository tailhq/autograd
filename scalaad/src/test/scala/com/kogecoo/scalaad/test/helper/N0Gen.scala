package scalaad.test.helper

import scalaad.Shape
import scalaad.graph._
import org.scalacheck.Gen


abstract class N0Gen[T] {

  def genVar0(valueGen: Gen[T]): Gen[Var]

  def genConst0(valueGen: Gen[T]): Gen[Const[Real]]

  final def genNode0(valueGen: Gen[T]): Gen[Expr[Real]] = {
    Gen.oneOf(
      genVar0(valueGen),
      genConst0(valueGen),
      genZero0(),
      genHalf0(),
      genOne0()
    )
  }

  final def genNonzeroNode0(valueGen: Gen[T]): Gen[Expr[Real]] = {
    Gen.oneOf(
      genVar0(valueGen),
      genConst0(valueGen),
      genHalf0(),
      genOne0()
    )
  }

  final def genNonVar0(valueGen: Gen[T]): Gen[Expr[Real]] = {
    Gen.oneOf(
      genConst0(valueGen),
      genZero0(),
      genHalf0(),
      genOne0()
    )
  }

  final def genNonzeroNonVar0(valueGen: Gen[T]): Gen[Expr[Real]] = {
    Gen.oneOf(
      genConst0(valueGen),
      genHalf0(),
      genOne0()
    )
  }

  final def genZero0(): Gen[Zero] = Gen.const(Zero(Shape()))

  final def genHalf0(): Gen[Half] = Gen.const(Half(Shape()))

  final def genOne0(): Gen[One] = Gen.const(One(Shape()))

}


