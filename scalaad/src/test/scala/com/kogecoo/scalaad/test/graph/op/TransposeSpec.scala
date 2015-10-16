package com.kogecoo.scalaad.test.graph.op

import com.kogecoo.scalaad.graph.{Var, Transpose, Node, Scalar}
import com.kogecoo.scalaad.test.helper.rule.{SeqFloatValueRule, ScalarIntValueRule}
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpSpec, UnaryOpSpecDef}
import com.kogecoo.scalaad.rule._

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import scala.language.higherKinds


// TODO: fruitful test
object TransposeSpec extends Properties("TransposeSpec") {

  implicit val scalarIntValueRule = new ScalarIntValueRule
  val scalarIntNodeGen = new ScalarIntNodeGen
  val scalarIntValueGen = new ScalarIntValueGen
  val scalarIntSpecGen = new UnaryOpSpec[Scalar, Int](new TransposeSpecDef[Scalar, Int], scalarIntNodeGen, scalarIntValueGen)

  implicit val seqFloatValueRule = new SeqFloatValueRule
  val seqFloatNodeGen = new SeqFloatNodeGen
  val seqFloatValueGen = new SeqFloatValueGen
  val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](new TransposeSpecDef[Seq, Float], seqFloatNodeGen, seqFloatValueGen)

  import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherProp._

  property("[Scalar, Int] - apply")              = scalarIntSpecGen.apply
  property("[Scalar, Int] - a.T deriv w.r.t. b") = scalarIntSpecGen.deriv
  property("[Scalar, Int] - a.T deriv w.r.t. a") = scalarIntSpecGen.derivSelf
  property("[Scalar, Int] - propagate")          = scalarIntSpecGen.propagate
  property("[Scalar, Int] - grad")               = scalarIntSpecGen.grad
  property("[Scalar, Int] - a.T deriv w.r.t. a.T") = forAll(scalarIntNodeGen.genVar) { (c: Var[Scalar, Int]) =>
    // FIXME: c.T.deriv(c.T) is not satisfy following condition
    val cT = c.T
    cT.deriv(cT) shouldBe scalarIntValueRule.zero(cT.apply())
  }

  property("[Seq, Float]  - apply")              = seqFloatSpecGen.apply
  property("[Seq, Float]  - a.T deriv w.r.t. b") = seqFloatSpecGen.deriv
  property("[Seq, Float]  - a.T deriv w.r.t. a") = seqFloatSpecGen.derivSelf
  property("[Seq, Float]  - propagate")          = seqFloatSpecGen.propagate
  property("[Seq, Float]  - grad")               = seqFloatSpecGen.grad

}


class TransposeSpecDef[U[_], T](implicit vr: ValueRule[U, T], num: Numeric[T]) extends UnaryOpSpecDef[U, T] {

  override def op(node: Node[U, T]): Node[U, T] = Transpose(node)

  override def applyExpectation(a: Node[U, T]): Value[U, T] = a() match {
    case x: NonContainerValue[U, T] => NonContainerValue[U, T](vr.transposeM(x.data))
    case x: ContainerValue[U, T]    => ContainerValue[U, T](vr.transposeS(x.data))
  }

  override def derivExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = vr.zero(b())

  override def derivSelfExpectation(a: Node[U, T]): Value[U, T] = {
    a match {
      case x: Var[U, T] => vr.one(a())
      case x => vr.zero(a())
    }
  }

  override def propagateExpectation(a: Node[U, T], b: Value[U, T]): Value[U, T] = {
    a match {
      case x: Var[U, T] => vr.one(b) * b
      case x            => vr.zero(b)
    }
  }

  override def gradExpectation(a: Node[U, T]): Value[U, T] = {
    a match {
      case _: Var[U, T] => vr.one
      case _            => vr.zero
    }
  }

}

