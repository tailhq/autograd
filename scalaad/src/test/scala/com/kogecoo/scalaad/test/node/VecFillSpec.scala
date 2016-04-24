package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.Shape1
import com.kogecoo.scalaad.graph.{V0, V1, V2, S1, Var0, VecFill}
import com.kogecoo.scalaad.test.{NodeSpecBase, SpecBackend, StdSpecBackend}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object StdVecFillSpec extends Properties("VecFill") with VecFillSpec with StdSpecBackend


trait VecFillSpec extends NodeSpecBase { self: Properties with SpecBackend =>

  import com.kogecoo.scalaad.test.SpecBackendHelper.Implicits._

  def expectApplyOp(a: V0, s: S1): T1 = const1(a.toT0, s)

  def deriv(a: T0): T0 = one0

  def op(a: V0, s: S1): V1 = VecFill(a, s)

  def op(argStr: String): String = s"vec($argStr)"

  property("eval") = forAll(genN0(), genS1()) { (a: V0, s: S1) =>
    op(a, s) shouldCloseTo expectApplyOp(a, s)
  }

  property(s"${op("node0")} forward w.r.t node0") = forAll(genN0(), genN0(), genS1()) { (a: V0, b: V0, s: S1) =>
    op(a, s).forward[V0, V1](b) shouldCloseTo zero1(s)
  }

  property(s"${op("var0")} forward w.r.t self") = forAll(genV0(), genS1()) { (a: Var0, s: S1) =>
    op(a, s).forward[V0, V1](a) shouldCloseTo one1(s)
  }


  property(s"${op("nonvar0")} reverse node0") = forAll(genNV0(), genN0(), genS1()) { (a: V0, b: V0, s: S1) =>
    op(a, s).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node1") = forAll(genNV0(), genN1()) { (a: V0, b: V1) =>
    op(a, b.shape).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node2") = forAll(genNV0(), genN2()) { (a: V0, b: V2) =>
    op(a, Shape1(b.shape._1)).reverse(b).size == 0
  }

  property(s"${op("var0")} reverse node0") = forAll(genV0(), genN0(), genS1()) { (a: Var0, b: V0, s: S1) =>
    val g = op(a, s).reverse(b)
    g(a).get.asInstanceOf[V1] shouldCloseTo const1(b.toT0, s)
  }

  property(s"${op("var0")} reverse node1") = forAll(genV0(), genN1()) { (a: Var0, b: V1) =>
    val g = op(a, b.shape).reverse(b)
    g(a).get.asInstanceOf[V1] shouldCloseTo b.toT1
  }

  property(s"${op("var0")} reverse node2") = forAll(genV0(), genN2()) { (a: Var0, b: V2) =>
    val g = op(a, Shape1(b.shape._1)).reverse(b)
    g(a).get.asInstanceOf[V2] shouldCloseTo b.toT2
  }

}

