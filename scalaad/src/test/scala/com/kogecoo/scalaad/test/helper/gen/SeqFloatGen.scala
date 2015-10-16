package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.oneOf
import org.scalacheck.Gen


class SeqFloatNodeGen extends GenNode[Seq, Float] {

  implicit private[this] val r = new SeqFloatValueRule

  override def genNode: Gen[Node[Seq, Float]]= {
    oneOf(genScalarConst, genContainerConst, genVar)
  }

  override def genVar: Gen[Var[Seq, Float]] = {
    for {
      v <- arbitrary[Seq[Float]] suchThat(_.nonEmpty)
    } yield Var[Seq, Float](v)
  }

  override def genScalarConst: Gen[ScalarConst[Seq, Float]] = {
    for {
      v <- arbitrary[Float]
    } yield ScalarConst[Seq, Float](v)
  }

  override def genContainerConst: Gen[ContainerConst[Seq, Float]] = {
    for {
      v <- arbitrary[Seq[Float]] suchThat(_.nonEmpty)
    } yield ContainerConst[Seq, Float](v)
  }

}

class SeqFloatValueGen extends GenValue[Seq, Float] {

  implicit private[this] val r = new SeqFloatValueRule

  override def genValue: Gen[Value[Seq, Float]] = {
    oneOf(genNonContainerValue, genContainerValue)
  }

  override def genNonContainerValue: Gen[NonContainerValue[Seq, Float]] = {
    for {
      v <- arbitrary[Float]
    } yield NonContainerValue[Seq, Float](v)
  }

  override def genContainerValue: Gen[ContainerValue[Seq, Float]] = {
    for {
      v <- arbitrary[Seq[Float]] suchThat(_.nonEmpty)
    } yield ContainerValue[Seq, Float](v)
  }

}
