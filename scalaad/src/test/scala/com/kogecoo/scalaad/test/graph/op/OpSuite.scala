package com.kogecoo.scalaad.test.graph.op

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherAssert._
import com.kogecoo.scalaad.rule.{ContainerValue, NonContainerValue}
import com.kogecoo.scalaad.test.helper.rule.ScalarIntComparerRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatCompareRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.ScalarIntValueRule.Implicits._
import org.scalatest.FunSuite


class OpSuite extends FunSuite {

  test("Add - Scalar[Int]") {

    val value42 = NonContainerValue[Scalar, Int](42)
    val cValue211 = ContainerValue[Scalar, Int](Scalar(211))

    val sc4 = ScalarConst(4)
    val sc3 = ScalarConst(3)
    val cc31 = ContainerConst[Scalar, Int](Scalar(31))
    val cc72 = ContainerConst[Scalar, Int](Scalar(72))
    val var7 = Var(Scalar(7))
    val var13 = Var(Scalar(13))

    // ScalarConst + ScalarConst
    val a1 = Add(sc4, sc3)

    a1.apply()              shouldBe 7
    a1.deriv(sc4)           shouldBe 0
    a1.deriv(sc3)           shouldBe 0
    a1.deriv(cc31)          shouldBe Scalar(0)
    a1.deriv(cc72)          shouldBe Scalar(0)
    a1.deriv(var7)          shouldBe Scalar(0)
    a1.deriv(var13)         shouldBe Scalar(0)
    a1.propagate(value42)   shouldBe 0
    a1.propagate(cValue211) shouldBe Scalar(0)

    // ScalarConst + ContainerConst
    val a2 = Add(sc4, cc72)

    a2.apply()              shouldBe Scalar(76)
    a2.deriv(sc4)           shouldBe 0
    a2.deriv(sc3)           shouldBe 0
    a2.deriv(cc31)          shouldBe Scalar(0)
    a2.deriv(cc72)          shouldBe Scalar(0)
    a2.deriv(var7)          shouldBe Scalar(0)
    a2.deriv(var13)         shouldBe Scalar(0)
    a2.propagate(value42)   shouldBe 0
    a2.propagate(cValue211) shouldBe Scalar(0)

    // ContaoinerConst + ScalarConst
    val a3 = Add(cc31, sc3)

    a3.apply()              shouldBe Scalar(34)
    a3.deriv(sc4)           shouldBe 0
    a3.deriv(sc3)           shouldBe 0
    a3.deriv(cc31)          shouldBe Scalar(0)
    a3.deriv(cc72)          shouldBe Scalar(0)
    a3.deriv(var7)          shouldBe Scalar(0)
    a3.deriv(var13)         shouldBe Scalar(0)
    a3.propagate(value42)   shouldBe 0
    a3.propagate(cValue211) shouldBe Scalar(0)

    // ContainerConst + ContainerConst
    val a4 = Add(cc31, cc72)

    a4.apply()              shouldBe Scalar(103)
    a4.deriv(sc4)           shouldBe 0
    a4.deriv(sc3)           shouldBe 0
    a4.deriv(cc31)          shouldBe Scalar(0)
    a4.deriv(cc72)          shouldBe Scalar(0)
    a4.deriv(var7)          shouldBe Scalar(0)
    a4.deriv(var13)         shouldBe Scalar(0)
    a4.propagate(value42)   shouldBe 0
    a4.propagate(cValue211) shouldBe Scalar(0)

    // above a1 - a4 is may be redundant

    // Var + ScalarConst
    val a5 = Add(var7, sc3)

    a5.apply()              shouldBe Scalar(10)
    a5.deriv(var7)          shouldBe Scalar(1)
    a5.deriv(sc3)           shouldBe 0
    a5.deriv(cc31)          shouldBe Scalar(0)
    a5.propagate(value42)   shouldBe 42
    a5.propagate(cValue211) shouldBe Scalar(211)

    // Var + ContainerConst
    val a6 = Add(var7, cc31)

    a6.apply()              shouldBe Scalar(7 + 31)
    a6.deriv(var7)          shouldBe Scalar(1)
    a6.deriv(sc3)           shouldBe 0
    a6.deriv(cc72)          shouldBe Scalar(0)
    a6.propagate(value42)   shouldBe 42
    a6.propagate(cValue211) shouldBe Scalar(211)

    // ScalarConst + Var
    val a7 = Add(sc4, var13)

    a7.apply()              shouldBe Scalar(17)
    a7.deriv(sc4)           shouldBe 0
    a7.deriv(var13)         shouldBe Scalar(1)
    a7.propagate(value42)   shouldBe 42
    a7.propagate(cValue211) shouldBe Scalar(211)

    // Var + ContainerConst
    val a8 = Add(cc31, var7)

    a6.apply()              shouldBe Scalar(7 + 31)
    a6.deriv(var7)          shouldBe Scalar(1)
    a6.deriv(sc3)           shouldBe 0
    a6.deriv(cc72)          shouldBe Scalar(0)
    a6.propagate(value42)   shouldBe 42
    a6.propagate(cValue211) shouldBe Scalar(211)

    // Var + Var
    val a9 = Add(var7, var13)

    a9.apply()              shouldBe Scalar(20)
    a9.deriv(var7)          shouldBe Scalar(1)
    a9.deriv(var13)         shouldBe Scalar(1)
    a9.propagate(value42)   shouldBe 84
    a9.propagate(cValue211) shouldBe Scalar(422)

    // more complex one
    // ((x + 3) + y), x = 7, y = 13
    val a10 = Add(Add(var7, sc3), var13)

    a10.apply()              shouldBe Scalar(23)
    a10.deriv(var7)          shouldBe Scalar(1)
    a10.deriv(var13)         shouldBe Scalar(1)
    a10.deriv(sc3)           shouldBe 0
    a10.propagate(value42)   shouldBe 84
    a10.propagate(cValue211) shouldBe Scalar(422)
  }

  test("Add - Seq[Float]") {

    val var71_3 = Var(Seq(71.0f, 3f))
    val var33_51 = Var(Seq(33.0f, 51.0f))
    val c12_42 = ContainerConst[Seq, Float](Seq(12.0f, 42.0f))
    val c63_21 = ContainerConst[Seq, Float](Seq(63.0f, 21.0f))
    val sc2 = ScalarConst[Seq, Float](2.0f)
    val sc5 =  ScalarConst[Seq, Float](5.0f)

    val value15 = NonContainerValue[Seq, Float](15.0f)
    val cValue8 = ContainerValue[Seq, Float](Seq(3.0f, 5.0f))

    // ContainerConst + ContainerConst
    val a1 = Add(c12_42, c63_21)

    a1.apply()            shouldBe Seq(75.0f, 63.0f)
    a1.deriv(c12_42)      shouldBe Seq(0f, 0f)
    a1.deriv(c63_21)      shouldBe Seq(0f, 0f)
    a1.propagate(value15) shouldBe 0
    a1.propagate(cValue8) shouldBe Seq(0.0f, 0.0f)

    // above a1 - a4 is may be redundant

    // Var + ContainerConst
    val a2 = Add(var71_3, c63_21)

    a2.apply()            shouldBe Seq(134f, 24f)
    a2.deriv(var71_3)     shouldBe Seq(1f, 1f)
    a2.deriv(c63_21)      shouldBe Seq(0f, 0f)
    a2.propagate(value15) shouldBe 15
    a2.propagate(cValue8) shouldBe Seq(3.0f, 5.0f)

    // ContainerConst + Var
    val a3 = Add(c12_42, var33_51)

    a3.apply()            shouldBe Seq(45f, 93f)
    a3.deriv(c12_42)      shouldBe Seq(0f, 0f)
    a3.deriv(var33_51)    shouldBe Seq(1f, 1f)
    a3.propagate(value15) shouldBe 15
    a3.propagate(cValue8) shouldBe Seq(3.0f, 5.0f)

    // Var + Var
    val a4 = Add(var71_3, var33_51)

    a4.apply()            shouldBe Seq(104f, 54f)
    a4.deriv(var71_3)     shouldBe Seq(1f, 1f)
    a4.deriv(var33_51)    shouldBe Seq(1f, 1f)
    a4.propagate(value15) shouldBe 30f
    a4.propagate(cValue8) shouldBe Seq(6.0f, 10.0f)

    // more complex one
    // ((x + 2) + y), x = [71, 3], y = [33, 51]
    val a5 = Add(Add(var71_3, sc2), var33_51)
    a5.apply()            shouldBe Seq(106f, 56f)
    a5.deriv(var71_3)     shouldBe Seq(1f, 1f)
    a5.deriv(var33_51)    shouldBe Seq(1f, 1f)
    a5.propagate(value15) shouldBe 30f
    a5.propagate(cValue8) shouldBe Seq(6.0f, 10.0f)
  }

  test("Sub - Seq[Float]") {

    val var71_3 = Var(Seq(71.0f, 3f))
    val var33_51 = Var(Seq(33.0f, 51.0f))
    val c12_42 = ContainerConst[Seq, Float](Seq(12.0f, 42.0f))
    val c63_21 = ContainerConst[Seq, Float](Seq(63.0f, 21.0f))
    val sc2 = ScalarConst(2.0f)

    val value15 = NonContainerValue[Seq, Float](15.0f)
    val cValue8 = ContainerValue[Seq, Float](Seq(3.0f, 5.0f))

    // ContainerConst - ContainerConst
    val a1 = Sub(c12_42, c63_21)

    a1.apply()            shouldBe Seq(12f - 63f, 42f - 21f)
    a1.deriv(c12_42)      shouldBe Seq(0f, 0f)
    a1.deriv(c63_21)      shouldBe Seq(0f, 0f)
    a1.propagate(value15) shouldBe 0f
    a1.propagate(cValue8) shouldBe Seq(0.0f, 0.0f)

    // Var - ContainerConst
    val a2 = Sub(var71_3, c63_21)

    a2.apply()            shouldBe Seq(71f - 63f, 3f - 21f)
    a2.deriv(var71_3)     shouldBe Seq(1f, 1f)
    a2.deriv(c63_21)      shouldBe Seq(0f, 0f)
    a2.propagate(value15) shouldBe 15f
    a2.propagate(cValue8) shouldBe Seq(3.0f, 5.0f)

    // ContainerConst - Var
    val a3 = Sub(c12_42, var33_51)

    a3.apply()            shouldBe Seq(12f - 33f, 42f - 51f)
    a3.deriv(c12_42)      shouldBe Seq(0f, 0f)
    a3.deriv(var33_51)    shouldBe Seq(-1f, -1f)
    a3.propagate(value15) shouldBe -15f
    a3.propagate(cValue8) shouldBe Seq(-3.0f, -5.0f)

    // Var - Var
    val a4 = Sub(var71_3, var33_51)

    a4.apply()            shouldBe Seq(71f - 33f, 3f - 51f)
    a4.deriv(var71_3)     shouldBe Seq(1f, 1f)
    a4.deriv(var33_51)    shouldBe Seq(-1f, -1f)
    a4.propagate(value15) shouldBe 0f
    a4.propagate(cValue8) shouldBe Seq(0f, 0f)

    // more complex one
    // ((x - 2) - y), x = [71, 3], y = [33, 51]
    val a5 = Sub(Sub(var71_3, sc2), var33_51)
    a5.apply()            shouldBe Seq(71f - 2f - 33f, 3f - 2f - 51f)
    a5.deriv(var71_3)     shouldBe Seq(1f, 1f)
    a5.deriv(var33_51)    shouldBe Seq(-1f, -1f)
    a5.propagate(value15) shouldBe 0f
    a5.propagate(cValue8) shouldBe Seq(0f, 0f)
  }

  test("Mul - Seq[Float]") {

    val var71_3 = Var[Seq, Float](Seq(71.0f, 3f))
    val var33_51 = Var[Seq, Float](Seq(33.0f, 51.0f))
    val c12_42 = ContainerConst[Seq, Float](Seq(12.0f, 42.0f))
    val c63_21 = ContainerConst[Seq, Float](Seq(63.0f, 21.0f))
    val sc2 = ScalarConst(2.0f)

    val value15 = NonContainerValue[Seq, Float](15.0f)
    val cValue3_5 = ContainerValue[Seq, Float](Seq(3.0f, 5.0f))

    // ContainerConst * ContainerConst
    val a1 = Mul(c12_42, c63_21)

    a1.apply()              shouldBe Seq(12f * 63f, 42f * 21f)
    a1.deriv(c12_42)        shouldBe Seq(0f, 0f)
    a1.deriv(c63_21)        shouldBe Seq(0f, 0f)
    a1.propagate(value15)   shouldBe Seq(0f, 0f)
    a1.propagate(cValue3_5) shouldBe Seq(0.0f, 0.0f)

    // above a1 - a4 is may be redundant

    // Var * ContainerConst
    val a2 = Mul[Seq, Float](var71_3, c63_21)

    a2.apply()              shouldBe Seq(71f * 63f, 3f * 21f)
    a2.deriv(var71_3)       shouldBe Seq(63f, 21f)
    a2.deriv(c63_21)        shouldBe Seq(0f, 0f)
    a2.propagate(value15)   shouldBe Seq(15f * 63f, 15f * 21f)
    a2.propagate(cValue3_5) shouldBe Seq(3.0f * 63f, 5.0f * 21f)

    // ContainerConst * Var
    val a3 = Mul(c12_42, var33_51)

    a3.apply()              shouldBe Seq(12f * 33f, 42f * 51f)
    a3.deriv(c12_42)        shouldBe Seq(0f, 0f)
    a3.deriv(var33_51)      shouldBe Seq(12f, 42f)
    a3.propagate(value15)   shouldBe Seq(12f * 15f, 42f * 15f)
    a3.propagate(cValue3_5) shouldBe Seq(12f * 3.0f , 42f * 5f)

    // Var * Var
    val a4 = Mul(var71_3, var33_51)

    a4.apply()              shouldBe Seq(71f * 33f, 3f * 51f)
    a4.deriv(var71_3)       shouldBe Seq(33f, 51f)
    a4.deriv(var33_51)      shouldBe Seq(71f, 3f)
    a4.propagate(value15)   shouldBe Seq(71f * 15f + 33f * 15f, 3f * 15f + 51f * 15f)
    a4.propagate(cValue3_5) shouldBe Seq((71f  + 33f) * 3f, (3f + 51f) * 5f)

    // more complex one
    // ((x * 2) * y), x = [71, 3], y = [33, 51]
    val a5 = Mul(Mul(var71_3, sc2), var33_51)
    a5.apply()              shouldBe Seq(71f * 2f * 33f, 3f * 2f * 51f)
    a5.deriv(var71_3)       shouldBe Seq(33f * 2f, 51f * 2f)
    a5.deriv(var33_51)      shouldBe Seq(71f * 2f, 3f * 2f)
    a5.propagate(value15)   shouldBe Seq((71f + 33f) * 2f * 15f, (3f + 51f) * 2f * 15f)
    a5.propagate(cValue3_5) shouldBe Seq((71f + 33f) * 2f * 3f, (3f + 51f) * 2f * 5f)
  }

  test("Div - Seq[Float]") {

    val var71_3 = Var[Seq, Float](Seq(71.0f, 3f))
    val var33_51 = Var[Seq, Float](Seq(33.0f, 51.0f))
    val c12_42 = ContainerConst[Seq, Float](Seq(12.0f, 42.0f))
    val c63_21 = ContainerConst[Seq, Float](Seq(63.0f, 21.0f))
    val sc2 = ScalarConst(2.0f)

    val value15 = NonContainerValue[Seq, Float](15.0f)
    val cValue3_5 = ContainerValue[Seq, Float](Seq(3.0f, 5.0f))

    // ContainerConst / ContainerConst
    val a1 = Div(c12_42, c63_21)

    a1.apply()              shouldBe Seq(12f / 63f, 42f / 21f)
    a1.deriv(c12_42)        shouldBe Seq(0f, 0f)
    a1.deriv(c63_21)        shouldBe Seq(0f, 0f)
    a1.propagate(value15)   shouldBe Seq(0f, 0f)
    a1.propagate(cValue3_5) shouldBe Seq(0.0f, 0.0f)

    // Var / ScalarConst
    val a2 = Div(var71_3, c63_21)

    a2.apply()              shouldBe Seq(71f / 63f, 3f / 21f)
    a2.deriv(var71_3)       shouldBe Seq(1f / 63f, 1f / 21f)
    a2.deriv(c63_21)        shouldBe Seq(0f, 0f)
    a2.propagate(value15)   shouldBe Seq(15f * 1f / 63f, 15f * 1f / 21f)
    a2.propagate(cValue3_5) shouldBe Seq(3.0f * 1f / 63f, 5.0f * 1f / 21f)

    // ScalarConst / Var
    val a3 = Div(c12_42, var33_51)

    a3.apply()              shouldBe Seq(12f / 33f, 42f / 51f)
    a3.deriv(c12_42)        shouldBe Seq(0f, 0f)
    a3.deriv(var33_51)      shouldBe Seq(-12f / 33f / 33f, -42f / 51f / 51f)
    a3.propagate(value15)   shouldBe Seq(-15f * 12f / 33f / 33f, -15 * 42f / 51f / 51f)
    a3.propagate(cValue3_5) shouldBe Seq(-3f * 12f / 33f / 33f, -5f * 42f / 51f / 51f)

    // Var / Var
    val a4 = Div(var71_3, var33_51)

    a4.apply()              shouldBe Seq(71f / 33f, 3f / 51f)
    a4.deriv(var71_3)       shouldBe Seq(1f / 33f, 1f / 51f)
    a4.deriv(var33_51)      shouldBe Seq(-71f / 33f / 33f, -3f / 51f / 51f)
    a4.propagate(value15)   shouldBe Seq(-15f * 71f / 33f / 33f + 15f / 33f, -15f * 3f / 51f / 51f + 15f / 51f)
    a4.propagate(cValue3_5) shouldBe Seq(-3f * 71f / 33f / 33f + 3f / 33f, -5f * 3f / 51f / 51f + 5f / 51f)

    // more complex one
    // ((x / 2) / y), x = [71, 3], y = [33, 51]
    val a5 = Div(Div(var71_3, sc2), var33_51)
    a5.apply()              shouldBe Seq(71f / 2f / 33f, 3f / 2f / 51f)
    a5.deriv(var71_3)       shouldBe Seq(1f / 33f / 2f, 1f / 51f / 2f)
    a5.deriv(var33_51)      shouldBe Seq(-71f / 2f / 33f / 33f, -3f / 2f / 51f / 51f)
    a5.propagate(value15)   shouldBe Seq(-15f * 71f / 2f / 33f / 33f + 15f / 33f / 2f, -15f * 3f / 2f / 51f / 51f + 15f / 51f / 2f)
    a5.propagate(cValue3_5) shouldBe Seq(-3f * 71f / 2f / 33f / 33f + 3f / 33f / 2f, -5f * 3f / 2f / 51f / 51f + 5f / 51f / 2f)
  }

  test("Pos - Seq[Float]") {

    val var71_3 = Var[Seq, Float](Seq(71.0f, 3f))
    val c12_42 = ContainerConst[Seq, Float](Seq(12.0f, 42.0f))
    val sc2 = ScalarConst(2.0f)

    val value15 = NonContainerValue[Seq, Float](15.0f)
    val cValue3_5 = ContainerValue[Seq, Float](Seq(3.0f, 5.0f))

    // +ContainerConst
    val a1 = Pos(c12_42)

    a1.apply()              shouldBe Seq(12f , 42f)
    a1.deriv(var71_3)       shouldBe Seq(0f, 0f)
    a1.deriv(c12_42)        shouldBe Seq(0f, 0f)
    a1.propagate(value15)   shouldBe 0f
    a1.propagate(cValue3_5) shouldBe Seq(0.0f, 0.0f)

    // +Var
    val a2 = Pos(var71_3)

    a2.apply()              shouldBe Seq(71f , 3f)
    a2.deriv(var71_3)       shouldBe Seq(1f, 1f)
    a2.deriv(c12_42)        shouldBe Seq(0f, 0f)
    a2.propagate(value15)   shouldBe 15f
    a2.propagate(cValue3_5) shouldBe Seq(3.0f, 5f)

  }

  test("Neg - Seq[Float]") {

    val var71_3 = Var[Seq, Float](Seq(71.0f, 3f))
    val c12_42 = ContainerConst[Seq, Float](Seq(12.0f, 42.0f))
    val sc2 = ScalarConst(2.0f)

    val value15 = NonContainerValue[Seq, Float](15.0f)
    val cValue3_5 = ContainerValue[Seq, Float](Seq(3.0f, 5.0f))

    // -ContainerConst
    val a1 = Neg(c12_42)

    a1.apply()              shouldBe Seq(-12f , -42f)
    a1.deriv(var71_3)       shouldBe Seq(0f, 0f)
    a1.deriv(c12_42)        shouldBe Seq(0f, 0f)
    a1.propagate(value15)   shouldBe 0f
    a1.propagate(cValue3_5) shouldBe Seq(0.0f, 0.0f)

    // -Var
    val a2 = Neg(var71_3)

    a2.apply()              shouldBe Seq(-71f , -3f)
    a2.deriv(var71_3)       shouldBe Seq(-1f, -1f)
    a2.deriv(c12_42)        shouldBe Seq(0f, 0f)
    a2.propagate(value15)   shouldBe -15f
    a2.propagate(cValue3_5) shouldBe Seq(-3.0f, -5f)

  }
}
