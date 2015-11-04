package com.kogecoo.scalaad.test.helper

import com.kogecoo.scalaad.graph._
import org.scalacheck.Gen


abstract class N0Gen[T] {

  def genVar0(valueGen: Gen[T]): Gen[Var0]

  def genConst0(valueGen: Gen[T]): Gen[Const0]

  final def genNode0(valueGen: Gen[T]): Gen[N0] = {
    Gen.oneOf(
      genVar0(valueGen),
      genConst0(valueGen),
      genZero0(),
      genHalf0(),
      genOne0()
    )
  }

  final def genNonzeroNode0(valueGen: Gen[T]): Gen[N0] = {
    Gen.oneOf(
      genVar0(valueGen),
      genConst0(valueGen),
      genHalf0(),
      genOne0()
    )
  }

  final def genNonVar0(valueGen: Gen[T]): Gen[N0] = {
    Gen.oneOf(
      genConst0(valueGen),
      genZero0(),
      genHalf0(),
      genOne0()
    )
  }

  final def genNonzeroNonVar0(valueGen: Gen[T]): Gen[N0] = {
    Gen.oneOf(
      genConst0(valueGen),
      genHalf0(),
      genOne0()
    )
  }

  final def genZero0(): Gen[Zero0] = Gen.const(Zero0())

  final def genHalf0(): Gen[Half0] = Gen.const(Half0())

  final def genOne0(): Gen[One0] = Gen.const(One0())

}


