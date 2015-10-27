package com.kogecoo.scalaad.test.graph.math.suite

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.value.ContainerValue
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherAssert._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue}

import scala.util.Random
import org.scalatest.FunSuite


class SinSuite extends FunSuite {

  implicit val seqFloatCompareRule = new SeqFloatSoftCompareRule

  test("Sin - Seq[Float]") {

    val n = 10
    val varSeq = Seq.fill[Float](n)(Random.nextFloat())
    val ccSeq = Seq.fill[Float](n)(Random.nextFloat())
    val sc = Random.nextFloat()
    val value = Random.nextFloat()
    val cValue = Seq.fill(n)(Random.nextFloat())

    val varNode = Var[Seq, Float](varSeq)
    val ccNode = ContainerConst[Seq, Float](ccSeq)
    val scNode = ScalarConst(sc)

    val valueRand= NonContainerValue[Seq, Float](value)
    val cValueRand = ContainerValue[Seq, Float](cValue)

    (0 to 100).foreach { i =>
      // ScalarConst
      val scSin= sin(ccNode)

      scSin.apply()               shouldBe ccSeq.map(scala.math.sin(_).toFloat)
      scSin.deriv(varNode)        shouldBe Seq.fill(n)(0f)
      scSin.deriv(ccNode)         shouldBe Seq.fill(n)(0f)
      scSin.deriv(scNode)         shouldBe Seq.fill(n)(0f)
      scSin.propagate(valueRand)  shouldBe Seq.fill(n)(0f)
      scSin.propagate(cValueRand) shouldBe Seq.fill(n)(0f)

      // ContainerConst
      val ccSin = sin(ccNode)

      ccSin.apply()               shouldBe ccSeq.map(scala.math.sin(_).toFloat)
      ccSin.deriv(varNode)        shouldBe Seq.fill(n)(0f)
      ccSin.deriv(ccNode)         shouldBe Seq.fill(n)(0f)
      ccSin.deriv(scNode)         shouldBe Seq.fill(n)(0f)
      ccSin.propagate(valueRand)  shouldBe Seq.fill(n)(0f)
      ccSin.propagate(cValueRand) shouldBe Seq.fill(n)(0f)

      // Var
      val varSin = sin(varNode)

      varSin.apply()               shouldBe varSeq.map(scala.math.sin(_).toFloat)
      varSin.deriv(varNode)        shouldBe varSeq.map { v => (scala.math.cos(v)).toFloat }
      varSin.deriv(ccNode)         shouldBe Seq.fill(n)(0f)
      varSin.deriv(scNode)         shouldBe Seq.fill(n)(0f)
      varSin.propagate(valueRand)  shouldBe varSeq.map { v => (value * scala.math.cos(v)).toFloat }
      varSin.propagate(cValueRand) shouldBe varSeq.map({ v => scala.math.cos(v) }).zip(cValue).map { case (a, b) => (a * b).toFloat }

    }

  }

}
