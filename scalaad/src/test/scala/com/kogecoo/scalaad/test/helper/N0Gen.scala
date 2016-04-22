package com.kogecoo.scalaad.test.helper

import com.kogecoo.scalaad.Shape0
import com.kogecoo.scalaad.graph._
import org.scalacheck.Gen
import shapeless.Nat._0


abstract class N0Gen[T] {

  def genVar0(valueGen: Gen[T]): Gen[Var[_0]]

  def genConst0(valueGen: Gen[T]): Gen[Const[_0]]

  final def genNode0(valueGen: Gen[T]): Gen[V0] = {
    Gen.oneOf(
      genVar0(valueGen),
      genConst0(valueGen),
      genZero0(),
      genHalf0(),
      genOne0()
    )
  }

  final def genNonzeroNode0(valueGen: Gen[T]): Gen[V0] = {
    Gen.oneOf(
      genVar0(valueGen),
      genConst0(valueGen),
      genHalf0(),
      genOne0()
    )
  }

  final def genNonVar0(valueGen: Gen[T]): Gen[V0] = {
    Gen.oneOf(
      genConst0(valueGen),
      genZero0(),
      genHalf0(),
      genOne0()
    )
  }

  final def genNonzeroNonVar0(valueGen: Gen[T]): Gen[V0] = {
    Gen.oneOf(
      genConst0(valueGen),
      genHalf0(),
      genOne0()
    )
  }

  final def genZero0(): Gen[Zero[_0]] = Gen.const(Zero(Shape0()))

  final def genHalf0(): Gen[Half[_0]] = Gen.const(Half(Shape0()))

  final def genOne0(): Gen[One[_0]] = Gen.const(One(Shape0()))

}


