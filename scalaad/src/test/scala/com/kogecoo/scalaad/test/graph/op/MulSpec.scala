package com.kogecoo.scalaad.test.graph.op

import com.kogecoo.scalaad.graph.{Var, Mul, Node, Scalar}
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.ScalarIntValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.ScalarIntComparerRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatCompareRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{BinaryOpSpec, BinaryOpSpecDef}
import com.kogecoo.scalaad.rule._
import org.scalacheck.Properties

import scala.language.higherKinds

object MulSpec extends Properties("MulSpec") {

  val scalarIntSpecGen = new BinaryOpSpec[Scalar, Int](new MulSpecDef[Scalar, Int], new ScalarIntNodeGen, new ScalarIntValueGen)

  val seqFloatSpecGen = new BinaryOpSpec[Seq, Float](new MulSpecDef[Seq, Float], new SeqFloatNodeGen, new SeqFloatValueGen)

  property("[Scalar, Int] - apply")                  = scalarIntSpecGen.apply()
  property("[Scalar, Int] - (a * b) deriv w.r.t. c") = scalarIntSpecGen.deriv()
  property("[Scalar, Int] - (a * b) deriv w.r.t. a") = scalarIntSpecGen.derivWrtLeft()
  property("[Scalar, Int] - (a * b) deriv w.r.t. b") = scalarIntSpecGen.derivWrtRight()
  property("[Scalar, Int] - (a * a) deriv w.r.t. a") = scalarIntSpecGen.derivWrtSelf()
  property("[Scalar, Int] - propagate value")        = scalarIntSpecGen.propagate()
  property("[Scalar, Int] - grad")                   = scalarIntSpecGen.grad()

  property("[Seq, Float]  - apply")                  = seqFloatSpecGen.apply()
  property("[Seq, Float]  - (a * b) deriv w.r.t. c") = seqFloatSpecGen.deriv()
  property("[Seq, Float]  - (a * b) deriv w.r.t. a") = seqFloatSpecGen.derivWrtLeft()
  property("[Seq, Float]  - (a * b) deriv w.r.t. a") = seqFloatSpecGen.derivWrtRight()
  property("[Seq, Float]  - (a * a) deriv w.r.t. a") = seqFloatSpecGen.derivWrtSelf()
  property("[Seq, Float]  - propagate")              = seqFloatSpecGen.propagate()
  property("[Seq, Float]  - grad")                   = seqFloatSpecGen.grad()

}


class MulSpecDef[U[_], T](implicit vr: ValueRule[U, T], num: Numeric[T]) extends BinaryOpSpecDef[U, T] {

  override def op(node: Node[U, T], other: Node[U, T]): Node[U, T] = Mul(node, other)

  override def applyExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = (a(), b()) match {
    case (x: NonContainerValue[U, T], y: NonContainerValue[U, T]) => NonContainerValue[U, T](vr.mulMM(x.data, y.data))
    case (x: NonContainerValue[U, T], y: ContainerValue[U, T])    => ContainerValue[U, T](vr.mulMS(x.data, y.data))
    case (x: ContainerValue[U, T],    y: NonContainerValue[U, T]) => ContainerValue[U, T](vr.mulSM(x.data, y.data))
    case (x: ContainerValue[U, T],    y: ContainerValue[U, T])    => ContainerValue[U, T](vr.mulSS(x.data, y.data))
  }

  override def derivExpectation(a: Node[U, T], b: Node[U, T], c: Node[U, T]): Value[U, T] = {
    vr.zero(c()) * a() + vr.zero(c()) * b()
  }

  override def derivWrtLeftExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = {
    a match {
      case x: Var[U, T] => vr.one(a()) * b()
      case _            => vr.zero(a()) * b()
    }
  }

  override def derivWrtRightExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = {
    b match {
      case x: Var[U, T] => vr.one(b()) * a()
      case _            => vr.zero(b()) * a()
    }
  }

  override def derivWrtSelfExpectation(a: Node[U, T]): Value[U, T] = {
    a match {
      case x: Var[U, T] => vr.one(a()) * a() + vr.one(a()) * a()
      case _            => vr.zero(a())
    }
  }

  override def propagateExpectation(a: Node[U, T], b: Node[U, T], c: Value[U, T]): Value[U, T] = {
    val ag = a match {
      case x: Var[U, T] => b() * c * vr.one(b() * c)
      case _            => b() * c * vr.zero(b() * c)
    }
    val bg = b match {
      case y: Var[U, T] => a() * c * vr.one(a() * c)
      case _            => a() * c * vr.zero(a() * c)
    }
    ag + bg
  }

  override def gradExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T] = {
    val ag = a match {
      case x: Var[U, T] => b() * vr.one
      case _            => vr.zero(b() * vr.one)
    }
    val bg = b match {
      case y: Var[U, T] => a() * vr.one
      case _            => vr.zero(a() * vr.one)
    }
    ag + bg
  }

}
