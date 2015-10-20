package com.kogecoo.scalaad.test.graph.op.spec

import com.kogecoo.scalaad.graph.{Div, Node, Scalar, Var}
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.ScalarIntComparerRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.ScalarIntValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatCompareRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{BinaryOpSpec, BinaryOpSpecDef}
import org.scalacheck.Properties

import scala.language.higherKinds

object DivSpec extends Properties("DivSpec") {

  val scalarIntSpecGen = new BinaryOpSpec[Scalar, Int](new DivSpecDef[Scalar, Int], new ScalarIntNodeGen, new ScalarIntValueGen)

  val seqFloatSpecGen = new BinaryOpSpec[Seq, Float](new DivSpecDef[Seq, Float], new SeqFloatNodeGen, new SeqFloatValueGen)

  val iRangeLimit = (x: Int) => -40000 < x || x < 40000
  val fRangeLimit = (x: Float) => (1-15f < x && x < 1e15f) || (-1e15f < x && x < -1e-15f)

  val iNonZero = (x: Int) => x != 0 && iRangeLimit(x) // (too small int)^2 sometimes makes 0
  val fNonZero = (x: Float) => fRangeLimit(x) // (too small float)^2 sometimes makes 0

  property("[Scalar, Int] - apply")                  = scalarIntSpecGen.apply(iRangeLimit, iNonZero)
  property("[Scalar, Int] - (a / b) deriv w.r.t. c") = scalarIntSpecGen.deriv(iRangeLimit, iNonZero)
  property("[Scalar, Int] - (a / b) deriv w.r.t. a") = scalarIntSpecGen.derivWrtLeft(iRangeLimit, iNonZero)
  property("[Scalar, Int] - (a / b) deriv w.r.t. b") = scalarIntSpecGen.derivWrtRight(iRangeLimit, iNonZero)
  property("[Scalar, Int] - (a / a) deriv w.r.t. a") = scalarIntSpecGen.derivWrtSelf(iNonZero)
  property("[Scalar, Int] - propagate value")        = scalarIntSpecGen.propagate(iRangeLimit, iNonZero)
  property("[Scalar, Int] - grad")                   = scalarIntSpecGen.grad(iRangeLimit, iNonZero)

  property("[Seq, Float]  - apply")                  = seqFloatSpecGen.apply(fRangeLimit, fNonZero)
  property("[Seq, Float]  - (a / b) deriv w.r.t. c") = seqFloatSpecGen.deriv(fRangeLimit, fNonZero)
  property("[Seq, Float]  - (a / b) deriv w.r.t. a") = seqFloatSpecGen.derivWrtLeft(fRangeLimit, fNonZero)
  property("[Seq, Float]  - (a / b) deriv w.r.t. b") = seqFloatSpecGen.derivWrtRight(fRangeLimit, fNonZero)
  property("[Seq, Float]  - (a / a) deriv w.r.t. a") = seqFloatSpecGen.derivWrtSelf(fNonZero)
  property("[Seq, Float]  - propagate")              = seqFloatSpecGen.propagate(fRangeLimit, fNonZero)
  property("[Seq, Float]  - grad")                   = seqFloatSpecGen.grad(fRangeLimit, fNonZero)

}


class DivSpecDef[U[_], T](implicit vr: ValueRule[U, T], num: Numeric[T]) extends BinaryOpSpecDef[U, T] {

  override def op(node: Node[U, T], other: Node[U, T]): Node[U, T] = Div(node, other)

  override def applyExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = (a(), b()) match {
    case (x: NonContainerValue[U, T], y: NonContainerValue[U, T]) => NonContainerValue[U, T](vr.divMM(x.data, y.data))
    case (x: NonContainerValue[U, T], y: ContainerValue[U, T])    => ContainerValue[U, T](vr.divMS(x.data, y.data))
    case (x: ContainerValue[U, T],    y: NonContainerValue[U, T]) => ContainerValue[U, T](vr.divSM(x.data, y.data))
    case (x: ContainerValue[U, T],    y: ContainerValue[U, T])    => ContainerValue[U, T](vr.divSS(x.data, y.data))
  }

  override def derivExpectation(a: Node[U, T], b: Node[U, T], c: Node[U, T]): Value[U, T] = {
    val bval = b()
    vr.zero(c()) / bval - vr.zero(c()) * a() / bval / bval
  }

  override def derivWrtLeftExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = {
    val aval = a()
    val bval = b()
    a match {
      case _: Var[U, T] => vr.one(aval) / bval - vr.zero(b()) * aval / bval / bval
      case _            => vr.zero(b())  / bval - vr.zero(b()) * aval / bval / bval
    }
  }

  override def derivWrtRightExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = {
    val aval = a()
    val bval = b()
    b match {
      case _: Var[U, T] => vr.zero(aval) / bval - vr.one(bval) * aval / bval / bval
      case _            => vr.zero(aval) / bval - vr.zero(bval) * aval / bval / bval
    }
  }

  override def derivWrtSelfExpectation(a: Node[U, T]): Value[U, T] = {
    val aval = a()
    a match {
      case _: Var[U, T] => (vr.one(aval) * aval - vr.one(aval) * a()) / aval / aval
      case _            => (vr.zero(aval) * aval - vr.zero(aval) * a()) / aval/ aval
    }
  }

  override def propagateExpectation(a: Node[U, T], b: Node[U, T], c: Value[U, T]): Value[U, T] = {
    val bval = b()
    val nextLeft = c / bval
    val nextRight = -c * a() / bval / bval

    val ag = a match {
      case _: Var[U, T] => nextLeft * vr.one(nextLeft)
      case _            => nextLeft * vr.zero(nextLeft)
    }
    val bg = b match {
      case y: Var[U, T] => nextRight * vr.one(nextRight)
      case _            => nextRight * vr.zero(nextRight)
    }
    ag + bg
  }

  override def gradExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = {
    val bval = b()

    val ag = a match {
      case _: Var[U, T] => vr.one / bval
      case _            => (vr.one / bval) * vr.zero
    }
    val bg = b match {
      case _: Var[U, T] => -vr.one * a() / bval / bval
      case _            => (-vr.one * a() / bval / bval) * vr.zero
    }
    ag + bg
  }

}
