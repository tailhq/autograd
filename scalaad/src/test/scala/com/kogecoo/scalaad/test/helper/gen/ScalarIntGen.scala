package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.rule.{Value, NonContainerValue, ContainerValue}
import com.kogecoo.scalaad.test.helper.rule.ScalarIntValueRule
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf
import org.scalacheck.Arbitrary.arbitrary


class ScalarIntNodeGen extends GenNode[Scalar, Int] {

  implicit private[this] val r = new ScalarIntValueRule

  override def genNode(restrict: Int => Boolean): Gen[Node[Scalar, Int]] = {
    oneOf(genScalarConst(restrict), genContainerConst(restrict), genVar(restrict))
  }

  override def genVar(restrict: Int => Boolean): Gen[Var[Scalar, Int]] = {
    for {
      v <- arbitrary[Int] suchThat restrict
    } yield Var[Scalar, Int](Scalar(v))
  }

  override def genScalarConst(restrict: Int => Boolean): Gen[ScalarConst[Scalar, Int]] = {
    for {
      v <- arbitrary[Int] suchThat restrict
    } yield ScalarConst[Scalar, Int](v)
  }

  override def genContainerConst(restrict: Int => Boolean): Gen[ContainerConst[Scalar, Int]] = {
    for {
      v <- arbitrary[Int] suchThat restrict
    } yield ContainerConst[Scalar, Int](Scalar(v))
  }

}

class ScalarIntValueGen extends GenValue[Scalar, Int] {

  implicit private[this] val r = new ScalarIntValueRule

  override def genValue(restrict: Int => Boolean): Gen[Value[Scalar, Int]] = {
    oneOf(genNonContainerValue(restrict), genContainerValue(restrict))
  }

  override def genNonContainerValue(restrict: Int => Boolean): Gen[NonContainerValue[Scalar, Int]] = {
    for {
      v <- arbitrary[Int] suchThat restrict
    } yield NonContainerValue[Scalar, Int](v)
  }

  override def genContainerValue(restrict: Int => Boolean): Gen[ContainerValue[Scalar, Int]] = {
    for {
      v <- arbitrary[Int] suchThat(restrict)
    } yield ContainerValue[Scalar, Int](Scalar(v))
  }

}
