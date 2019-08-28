package io.github.tailabs.autograd.test.graph.math.suite

import io.github.tailabs.autograd.graph._
import io.github.tailabs.autograd.value.ContainerValue
import io.github.tailabs.autograd.test.helper.matcher.ValueMatcherAssert._
import io.github.tailabs.autograd.test.helper.rule.SeqFloatSoftCompareRule
import io.github.tailabs.autograd.test.helper.rule.SeqFloatValueRule.Implicits._
import io.github.tailabs.autograd.value.{ContainerValue, NonContainerValue}

import scala.util.Random
import org.scalatest.FunSuite


class SinSuite extends FunSuite {

  implicit val seqFloatCompareRule = new SeqFloatSoftCompareRule

  test("Sin - Seq[Float]") {

    val n = 10
    val varSeq1 = Seq.fill[Float](n)(Random.nextFloat())
    val varSeq2 = Seq.fill[Float](n)(Random.nextFloat())
    val ccSeq = Seq.fill[Float](n)(Random.nextFloat())
    val sc = Random.nextFloat()
    val value = Random.nextFloat()
    val cValue = Seq.fill(n)(Random.nextFloat())

    val varNode1 = Var[Seq, Float](varSeq1)
    val varNode2 = Var[Seq, Float](varSeq2)
    val ccNode = ContainerConst[Seq, Float](ccSeq)
    val scNode = ScalarConst(sc)

    val valueRand= NonContainerValue[Seq, Float](value)
    val cValueRand = ContainerValue[Seq, Float](cValue)

    (0 to 100).foreach { i =>
      // ScalarConst
      val scSin= sin(ccNode)

      scSin.apply()               shouldBe ccSeq.map(scala.math.sin(_).toFloat)
      scSin.deriv(varNode1)       shouldBe Seq.fill(n)(0f)
      scSin.propagate(valueRand)  shouldBe Seq.fill(n)(0f)
      scSin.propagate(cValueRand) shouldBe Seq.fill(n)(0f)

      // ContainerConst
      val ccSin = sin(ccNode)

      ccSin.apply()               shouldBe ccSeq.map(scala.math.sin(_).toFloat)
      ccSin.deriv(varNode1)       shouldBe Seq.fill(n)(0f)
      ccSin.propagate(valueRand)  shouldBe Seq.fill(n)(0f)
      ccSin.propagate(cValueRand) shouldBe Seq.fill(n)(0f)

      // Var
      val varSin = sin(varNode1)

      varSin.apply()               shouldBe varSeq1.map(scala.math.sin(_).toFloat)
      varSin.deriv(varNode1)       shouldBe varSeq1.map { v => scala.math.cos(v).toFloat }
      varSin.deriv(varNode2)       shouldBe Seq.fill(n)(0f)
      varSin.propagate(valueRand)  shouldBe varSeq1.map { v => (value * scala.math.cos(v)).toFloat }
      varSin.propagate(cValueRand) shouldBe varSeq1.map({ v => scala.math.cos(v) }).zip(cValue).map { case (a, b) => (a * b).toFloat }

    }

  }

}
