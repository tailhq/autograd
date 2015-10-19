package com.kogecoo.scalaad.test.graph.op

import com.kogecoo.scalaad.graph.{Var, Add, Node, Scalar}
import com.kogecoo.scalaad.test.helper.rule.ScalarIntValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.ScalarIntComparerRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatCompareRule.Implicits._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.specgen.{BinaryOpSpec, BinaryOpSpecDef}
import com.kogecoo.scalaad.rule._
import org.scalacheck.Properties

import scala.language.higherKinds

object AddSpec extends Properties("AddSpec") {

  val scalarIntSpecGen = new BinaryOpSpec[Scalar, Int](new AddSpecDef[Scalar, Int], new ScalarIntNodeGen, new ScalarIntValueGen)

  val seqFloatSpecGen = new BinaryOpSpec[Seq, Float](new AddSpecDef[Seq, Float], new SeqFloatNodeGen, new SeqFloatValueGen)

  property("[Scalar, Int] - apply")                  = scalarIntSpecGen.apply()
  property("[Scalar, Int] - (a + b) deriv w.r.t. c") = scalarIntSpecGen.deriv()
  property("[Scalar, Int] - (a + b) deriv w.r.t. a") = scalarIntSpecGen.derivWrtLeft()
  property("[Scalar, Int] - (a + b) deriv w.r.t. b") = scalarIntSpecGen.derivWrtRight()
  property("[Scalar, Int] - (a + a) deriv w.r.t. a") = scalarIntSpecGen.derivWrtSelf()
  property("[Scalar, Int] - propagate value")        = scalarIntSpecGen.propagate()
  property("[Scalar, Int] - grad")                   = scalarIntSpecGen.grad()

  property("[Seq, Float]  - apply")                  = seqFloatSpecGen.apply()
  property("[Seq, Float]  - (a + b) deriv w.r.t. c") = seqFloatSpecGen.deriv()
  property("[Seq, Float]  - (a + b) deriv w.r.t. a") = seqFloatSpecGen.derivWrtLeft()
  property("[Seq, Float]  - (a + b) deriv w.r.t. a") = seqFloatSpecGen.derivWrtRight()
  property("[Seq, Float]  - (a + a) deriv w.r.t. a") = seqFloatSpecGen.derivWrtSelf()
  property("[Seq, Float]  - propagate")              = seqFloatSpecGen.propagate()
  property("[Seq, Float]  - grad")                   = seqFloatSpecGen.grad()

}


class AddSpecDef[U[_], T](implicit vr: ValueRule[U, T], num: Numeric[T]) extends BinaryOpSpecDef[U, T] {

  override def op(node: Node[U, T], other: Node[U, T]): Node[U, T] = Add(node, other)

  override def applyExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = (a(), b()) match {
    case (x: NonContainerValue[U, T], y: NonContainerValue[U, T]) => NonContainerValue[U, T](vr.addMM(x.data, y.data))
    case (x: NonContainerValue[U, T], y: ContainerValue[U, T])    => ContainerValue[U, T](vr.addMS(x.data, y.data))
    case (x: ContainerValue[U, T],    y: NonContainerValue[U, T]) => ContainerValue[U, T](vr.addSM(x.data, y.data))
    case (x: ContainerValue[U, T],    y: ContainerValue[U, T])    => ContainerValue[U, T](vr.addSS(x.data, y.data))
  }

  override def derivExpectation(a: Node[U, T], b: Node[U, T], c: Node[U, T]): Value[U, T] = vr.zero(c())

  override def derivWrtLeftExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = {
    a match {
      case x: Var[U, T] => vr.one(a())
      case _            => vr.zero(a())
    }
  }

  override def derivWrtRightExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = {
    b match {
      case x: Var[U, T] => vr.one(b())
      case _            => vr.zero(b())
    }
  }

  override def derivWrtSelfExpectation(a: Node[U, T]): Value[U, T] = {
    a match {
      case x: Var[U, T] => vr.one(a()) + vr.one(a())
      case _            => vr.zero(a())
    }
  }

  override def propagateExpectation(a: Node[U, T], b: Node[U, T], c: Value[U, T]): Value[U, T] = {
    val ag = a match {
      case x: Var[U, T] => vr.one(c) * c
      case _            => vr.zero(c)
    }
    val bg = b match {
      case y: Var[U, T] => vr.one(c) * c
      case _            => vr.zero(c)
    }
    ag + bg
  }

  override def gradExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = {
    val ag = a match {
      case x: Var[U, T] => vr.one
      case _            => vr.zero
    }
    val bg = b match {
      case y: Var[U, T] => vr.one
      case _            => vr.zero
    }
    ag + bg
  }

}
