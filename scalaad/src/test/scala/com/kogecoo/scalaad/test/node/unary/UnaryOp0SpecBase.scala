package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{N0, N1, N2, Var0}
import com.kogecoo.scalaad.test.{NodeSpecBase, SpecBackend}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


trait UnaryOp0SpecBase extends NodeSpecBase { self: Properties with SpecBackend =>

  import com.kogecoo.scalaad.test.SpecBackendHelper.Implicits._

  def op(a: N0): N0

  def op(argStr: String): String

  def deriv(a: T0): T0


  def expectApplyOp(a: N0): T0

  def expectReverse0(a: Var0, b: N0): T0 = mul(b.toT0, deriv(a.toT0))

  def expectReverse1(a: Var0, b: N1): T1 = broadcast1(b.toT1, mul(_, deriv(a.toT0)))

  def expectReverse2(a: Var0, b: N2): T2 = broadcast2(b.toT2, mul(_, deriv(a.toT0)))


  def genArgN0ForSpecBase: Gen[N0] = genN0()

  def genArgNV0ForSpecBase: Gen[N0] = genNV0()

  def genArgV0ForSpecBase: Gen[Var0] = genV0()

  def genN0ForSpecBase: Gen[N0] = genN0()

  def genN1ForSpecBase: Gen[N1] = genN1()

  def genN2ForSpecBase: Gen[N2] = genN2()



  property("eval") = forAll(genArgN0ForSpecBase) { (a: N0) =>
    op(a) shouldCloseTo expectApplyOp(a)
  }

  property(s"${op("node0")} forward w.r.t node0") = forAll(genArgN0ForSpecBase, genN0ForSpecBase) { (a: N0, b: N0) =>
    op(a).forward[N0, N0](b) shouldCloseTo zero0
  }

  property(s"${op("node0")} forward w.r.t node1") = forAll(genArgN0ForSpecBase, genN1ForSpecBase) { (a: N0, b: N1) =>
    op(a).forward[N1, N1](b) shouldCloseTo zero1(b)
  }

  property(s"${op("var0")} forward w.r.t self") = forAll(genArgV0ForSpecBase) { (a: Var0) =>
    op(a).forward[N0, N0](a) shouldCloseTo deriv(a.toT0)
  }

  property(s"${op("node0")} forward w.r.t node2") = forAll(genArgN0ForSpecBase, genN2ForSpecBase) { (a: N0, b: N2) =>
    op(a).forward[N2, N2](b) shouldCloseTo zero2(b)
  }

  property(s"${op("nonvar0")} reverse node0") = forAll(genArgNV0ForSpecBase, genN0ForSpecBase) { (a: N0, b: N0) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node1") = forAll(genArgNV0ForSpecBase, genN1ForSpecBase) { (a: N0, b: N1) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node2") = forAll(genArgNV0ForSpecBase, genN2ForSpecBase) { (a: N0, b: N2) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("var0")} reverse node0") = forAll(genArgV0ForSpecBase, genN0ForSpecBase) { (a: Var0, b: N0) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N0] shouldCloseTo expectReverse0(a, b)
  }

  property(s"${op("var0")} reverse node1") = forAll(genArgV0ForSpecBase, genN1ForSpecBase) { (a: Var0, b: N1) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N1] shouldCloseTo expectReverse1(a, b)
  }

  property(s"${op("var0")} reverse node2") = forAll(genArgV0ForSpecBase, genN2ForSpecBase) { (a: Var0, b: N2) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N2] shouldCloseTo expectReverse2(a, b)
  }

}





