package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.TestUtil._
import com.kogecoo.scalaad.rule.{ContainerValue, NonContainerValue, ValueRuleExample}
import org.scalatest.FunSuite

import scala.language.higherKinds


class ScalarConstSpec extends FunSuite {

  test("ScalarConst - Scalar[Int]") {
    implicit val r = ValueRuleExample.valueRuleScalarInt
    val c = ScalarConst[Scalar, Int](10)
    val prop = NonContainerValue[Scalar, Int](20)

    c.apply()               shouldBe 10
    c.deriv(Var(Scalar(3))) shouldBe 0
    c.deriv(c)              shouldBe 0
    c.propagate(prop)       shouldBe 0
    c.grad()                shouldBe 0
  }

  test("ScalarConst - Seq[Float]") {
    implicit val r = ValueRuleExample.valueRuleSeqFloat
    val c = ScalarConst[Seq, Float](3.0f)
    val dummyVar = Var(Seq(4.0f, 5.0f))
    val prop = NonContainerValue[Seq, Float](15.5f)

    c.apply()         shouldBe 3.0f
    c.deriv(dummyVar) shouldBe 0f
    c.deriv(c)        shouldBe 0f
    c.propagate(prop) shouldBe 0f
    c.grad()          shouldBe 0f
  }

  test("ContainerConst - Scalar[Int]") {
    implicit val r = ValueRuleExample.valueRuleScalarInt
    val c = ContainerConst[Scalar, Int](Scalar(17))
    val dummyVar = Var(Scalar(7))
    val prop = ContainerValue[Scalar, Int](Scalar(11))

    c.apply()         shouldBe Scalar(17)
    c.deriv(dummyVar) shouldBe Scalar(0)
    c.deriv(c)        shouldBe Scalar(0)
    c.propagate(prop) shouldBe Scalar(0)
    c.grad()          shouldBe Scalar(0)
  }

  test("ContainerConst - Seq[Float]") {
    implicit val r = ValueRuleExample.valueRuleSeqFloat
    val c = ContainerConst[Seq, Float](Seq(1.1f, 2.2f))
    val dummyVar = Var(Seq(3.3f, 4.4f))
    val prop = ContainerValue[Seq, Float](Seq(1.0f, 2.0f))

    c.apply()         shouldBe 3.0f
    c.deriv(dummyVar) shouldBe Seq(0f, 0f)
    c.deriv(c)        shouldBe Seq(0f, 0f)
    c.propagate(prop) shouldBe Seq(0f, 0f)
    c.grad()          shouldBe Seq(0f, 0f)

  }

}

