package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{V0, V1, V2, Var0}
import com.kogecoo.scalaad.test.{NodeSpecBase, SpecBackend}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


trait UnaryOp0SpecBase extends NodeSpecBase { self: Properties with SpecBackend =>

  import com.kogecoo.scalaad.test.SpecBackendHelper.Implicits._

  def op(a: V0): V0

  def op(argStr: String): String

  def deriv(a: T0): T0


  def expectApplyOp(a: V0): T0

  def expectReverse0(a: Var0, b: V0): T0 = mul(b.toT0, deriv(a.toT0))

  def expectReverse1(a: Var0, b: V1): T1 = broadcast1(b.toT1, mul(_, deriv(a.toT0)))

  def expectReverse2(a: Var0, b: V2): T2 = broadcast2(b.toT2, mul(_, deriv(a.toT0)))


  def genArgN0ForSpecBase: Gen[V0] = genN0()

  def genArgNV0ForSpecBase: Gen[V0] = genNV0()

  def genArgV0ForSpecBase: Gen[Var0] = genV0()

  def genN0ForSpecBase: Gen[V0] = genN0()

  def genN1ForSpecBase: Gen[V1] = genN1()

  def genN2ForSpecBase: Gen[V2] = genN2()



  property("eval") = forAll(genArgN0ForSpecBase) { (a: V0) =>
    op(a) shouldCloseTo expectApplyOp(a)
  }

  property(s"${op("node0")} forward w.r.t node0") = forAll(genArgN0ForSpecBase, genN0ForSpecBase) { (a: V0, b: V0) =>
    op(a).forward[V0, V0](b) shouldCloseTo zero0
  }

  property(s"${op("node0")} forward w.r.t node1") = forAll(genArgN0ForSpecBase, genN1ForSpecBase) { (a: V0, b: V1) =>
    op(a).forward[V1, V1](b) shouldCloseTo zero1(b)
  }

  property(s"${op("var0")} forward w.r.t self") = forAll(genArgV0ForSpecBase) { (a: Var0) =>
    op(a).forward[V0, V0](a) shouldCloseTo deriv(a.toT0)
  }

  property(s"${op("node0")} forward w.r.t node2") = forAll(genArgN0ForSpecBase, genN2ForSpecBase) { (a: V0, b: V2) =>
    op(a).forward[V2, V2](b) shouldCloseTo zero2(b)
  }

  property(s"${op("nonvar0")} reverse node0") = forAll(genArgNV0ForSpecBase, genN0ForSpecBase) { (a: V0, b: V0) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node1") = forAll(genArgNV0ForSpecBase, genN1ForSpecBase) { (a: V0, b: V1) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node2") = forAll(genArgNV0ForSpecBase, genN2ForSpecBase) { (a: V0, b: V2) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("var0")} reverse node0") = forAll(genArgV0ForSpecBase, genN0ForSpecBase) { (a: Var0, b: V0) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[V0] shouldCloseTo expectReverse0(a, b)
  }

  property(s"${op("var0")} reverse node1") = forAll(genArgV0ForSpecBase, genN1ForSpecBase) { (a: Var0, b: V1) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[V1] shouldCloseTo expectReverse1(a, b)
  }

  property(s"${op("var0")} reverse node2") = forAll(genArgV0ForSpecBase, genN2ForSpecBase) { (a: Var0, b: V2) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[V2] shouldCloseTo expectReverse2(a, b)
  }

}





