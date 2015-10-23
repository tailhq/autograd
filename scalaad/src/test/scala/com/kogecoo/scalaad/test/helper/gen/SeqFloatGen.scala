package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.oneOf
import org.scalacheck.Gen


class SeqFloatNodeGen extends GenNode[Seq, Float] {

  implicit private[this] val r = new SeqFloatValueRule

  override def genVarWithSource(restrict: Float => Boolean): Gen[(Var[Seq, Float], Seq[Float])] = {
    for {
      v <- arbitrary[Seq[Float]] suchThat { a => a.nonEmpty && a.forall(restrict) }
    } yield (Var[Seq, Float](v), v)
  }

  override def genScalarConstWithSource(restrict: Float => Boolean): Gen[(ScalarConst[Seq, Float], Float)] = {
    for {
      v <- arbitrary[Float] suchThat restrict
    } yield (ScalarConst[Seq, Float](v), v)
  }

  override def genContainerConstWithSource(restrict: Float => Boolean): Gen[(ContainerConst[Seq, Float], Seq[Float])] = {
    for {
      v <- arbitrary[Seq[Float]] suchThat(_.nonEmpty)
    } yield (ContainerConst[Seq, Float](v), v)
  }

}


class SeqFloatValueGen extends GenValue[Seq, Float] {

  implicit private[this] val r = new SeqFloatValueRule

  override def genNonContainerValueWithSource(restrict: Float => Boolean): Gen[(NonContainerValue[Seq, Float], Float)] = {
    for {
      v <- arbitrary[Float] suchThat restrict
    } yield (NonContainerValue[Seq, Float](v), v)
  }

  override def genContainerValueWithSource(restrict: Float => Boolean): Gen[(ContainerValue[Seq, Float], Seq[Float])] = {
    for {
      v <- arbitrary[Seq[Float]] suchThat { a => a.nonEmpty && a.forall(restrict) }
    } yield (ContainerValue[Seq, Float](v), v)
  }

}
