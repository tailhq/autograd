package com.kogecoo.scalaad.test.graph.op

import com.kogecoo.scalaad.graph.{Var, Neg, Node, Scalar}
import com.kogecoo.scalaad.test.helper.rule.{SeqFloatValueRule, ScalarIntValueRule}
import ScalarIntValueRule.Implicits._
import SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpSpec, UnaryOpSpecDef}
import com.kogecoo.scalaad.rule._
import org.scalacheck.Properties

import scala.language.higherKinds


object NegSpec extends Properties("NegSpec") {

  val scalarIntSpecGen = new UnaryOpSpec[Scalar, Int](new NegSpecDef[Scalar, Int], new ScalarIntNodeGen, new ScalarIntValueGen)

  val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](new NegSpecDef[Seq, Float], new SeqFloatNodeGen, new SeqFloatValueGen)

  property("[Scalar, Int] - apply")             = scalarIntSpecGen.apply
  property("[Scalar, Int] - -a deriv w.r.t. b") = scalarIntSpecGen.deriv
  property("[Scalar, Int] - -a deriv w.r.t. a") = scalarIntSpecGen.derivSelf
  property("[Scalar, Int] - propagate")         = scalarIntSpecGen.propagate
  property("[Scalar, Int] - grad")              = scalarIntSpecGen.grad

  property("[Seq, Float]  - apply")             = seqFloatSpecGen.apply
  property("[Seq, Float]  - -a deriv w.r.t. b") = seqFloatSpecGen.deriv
  property("[Seq, Float]  - -a deriv w.r.t. a") = seqFloatSpecGen.derivSelf
  property("[Seq, Float]  - propagate")         = seqFloatSpecGen.propagate
  property("[Seq, Float]  - grad")              = seqFloatSpecGen.grad

}


class NegSpecDef[U[_], T](implicit vr: ValueRule[U, T], num: Numeric[T]) extends UnaryOpSpecDef[U, T] {

  override def op(node: Node[U, T]): Node[U, T] = Neg(node)

  override def applyExpectation(a: Node[U, T]): Value[U, T] = a() match {
    case x: NonContainerValue[U, T] => NonContainerValue[U, T](vr.negM(x.data))
    case x: ContainerValue[U, T]    => ContainerValue[U, T](vr.negS(x.data))
  }

  override def derivExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = vr.zero(b())

  override def derivSelfExpectation(a: Node[U, T]): Value[U, T] = {
    a match {
      case x: Var[U, T] => -vr.one(a())
      case x            => vr.zero(a())
    }
  }

  override def propagateExpectation(a: Node[U, T], b: Value[U, T]): Value[U, T] = {
    a match {
      case x: Var[U, T] => -vr.one(b) * b
      case x            => vr.zero(b)
    }
  }

  override def gradExpectation(a: Node[U, T]): Value[U, T] = {
    a match {
      case x: Var[U, T] => -vr.one
      case x            => vr.zero
    }
  }

}
