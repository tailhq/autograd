package io.github.tailabs.autograd.test.graph

import io.github.tailabs.autograd.graph._
import io.github.tailabs.autograd.test.helper.matcher.ValueMatcherAssert._
import io.github.tailabs.autograd.test.helper.rule.SeqFloatExactCompareRule
import io.github.tailabs.autograd.test.helper.rule.SeqFloatValueRule.Implicits._
import io.github.tailabs.autograd.value.{ContainerValue, NonContainerValue}
import org.scalatest.FunSuite


class ContainerConstSuite extends FunSuite {

  implicit val seqFloatCompareRule = new SeqFloatExactCompareRule

  test("ContainerConst - Seq[Float]") {

    val var12_3 = Var[Seq, Float](Seq(12.0f, 3f))
    val c45_6 = ContainerConst[Seq, Float](Seq(45.0f, 6.0f))
    val sc7 = ScalarConst(7.0f)

    val value89 = NonContainerValue[Seq, Float](89.0f)
    val cValue10_11 = ContainerValue[Seq, Float](Seq(10.0f, 11.0f))

    val a1 = ContainerConst[Seq, Float](Seq(3.1f, 41.5f))

    a1.apply()                shouldBe Seq(3.1f, 41.5f)
    a1.deriv(var12_3)         shouldBe Seq(0f, 0f)
    a1.propagate(value89)     shouldBe Seq(0f, 0f)
    a1.propagate(cValue10_11) shouldBe Seq(0.0f, 0.0f)

  }

}
