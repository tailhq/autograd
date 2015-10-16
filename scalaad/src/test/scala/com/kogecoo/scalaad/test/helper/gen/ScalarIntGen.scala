package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.rule.{Value, NonContainerValue, ContainerValue}
import com.kogecoo.scalaad.test.helper.rule.ScalarIntValueRule
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf
import org.scalacheck.Arbitrary.arbitrary


class ScalarIntNodeGen extends GenNode[Scalar, Int] {

  implicit private[this] val r = new ScalarIntValueRule

  override def genNode: Gen[Node[Scalar, Int]] = {
    oneOf(genScalarConst, genContainerConst, genVar)
  }

  override def genVar: Gen[Var[Scalar, Int]] = {
    for (v <- arbitrary[Int]) yield Var[Scalar, Int](Scalar(v))
  }

  override def genScalarConst: Gen[ScalarConst[Scalar, Int]] = {
    for (v <- arbitrary[Int]) yield ScalarConst[Scalar, Int](v)
  }

  override def genContainerConst: Gen[ContainerConst[Scalar, Int]] = {
    for (v <- arbitrary[Int]) yield ContainerConst[Scalar, Int](Scalar(v))
  }

}

class ScalarIntValueGen extends GenValue[Scalar, Int] {

  implicit private[this] val r = new ScalarIntValueRule

  override def genValue: Gen[Value[Scalar, Int]] = {
    oneOf(genNonContainerValue, genContainerValue)
  }

  override def genNonContainerValue: Gen[NonContainerValue[Scalar, Int]] = {
    for (v <- arbitrary[Int]) yield NonContainerValue[Scalar, Int](v)
  }

  override def genContainerValue: Gen[ContainerValue[Scalar, Int]] = {
    for (v <- arbitrary[Int]) yield ContainerValue[Scalar, Int](Scalar(v))
  }

}
