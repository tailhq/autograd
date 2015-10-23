package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.rule.{NonContainerValue, ContainerValue}
import com.kogecoo.scalaad.test.helper.rule.ScalarIntValueRule
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary


class ScalarIntNodeGen extends GenNode[Scalar, Int] {

  implicit private[this] val r = new ScalarIntValueRule


  override def genVarWithSource(restrict: Int => Boolean): Gen[(Var[Scalar, Int], Scalar[Int])] = {
    for {
      v <- arbitrary[Int] suchThat restrict
    } yield (Var[Scalar, Int](Scalar(v)), Scalar(v))
  }

  override def genScalarConstWithSource(restrict: Int => Boolean): Gen[(ScalarConst[Scalar, Int], Int)] = {
    for {
      v <- arbitrary[Int] suchThat restrict
    } yield (ScalarConst[Scalar, Int](v), v)
  }

  override def genContainerConstWithSource(restrict: Int => Boolean): Gen[(ContainerConst[Scalar, Int], Scalar[Int])] = {
    for {
      v <- arbitrary[Int] suchThat restrict
    } yield (ContainerConst[Scalar, Int](Scalar(v)), Scalar[Int](v))
  }
}

class ScalarIntValueGen extends GenValue[Scalar, Int] {

  implicit private[this] val r = new ScalarIntValueRule


  override def genNonContainerValueWithSource(restrict: Int => Boolean): Gen[(NonContainerValue[Scalar, Int], Int)] = {
    for {
      v <- arbitrary[Int] suchThat restrict
    } yield (NonContainerValue[Scalar, Int](v), v)
  }

  override def genContainerValueWithSource(restrict: Int => Boolean): Gen[(ContainerValue[Scalar, Int], Scalar[Int])] = {
    for {
      v <- arbitrary[Int] suchThat(restrict)
    } yield (ContainerValue[Scalar, Int](Scalar(v)), Scalar(v))
  }

}
