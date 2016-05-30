package com.kogecoo.scalaad.algorithm

import com.kogecoo.scalaad.analyze._
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.op._
import com.kogecoo.scalaad.{S0, S1, S2, Shape}

import scala.Predef.{any2stringadd => _}


trait Analyze[S <: Shape] {

  def analyze(n: ValueExpr[S], analyzing: Analyzing): Param[S]

}

object Analyze {

  implicit def analyze0[S <: Shape]: Analyze[S0] = new Analyze[S0] {

    def analyze(n: ValueExpr[S0], a: Analyzing): Param[S0] = {
      val p = Param[S0]()

      n match {
        case _: Var0    => a.addParam(p, n)
        case _: ArbVar0 => a.addParam(p, n)
        case _: Zero0   => a.addParam(p, n)
        case _: Half0   => a.addParam(p, n)
        case _: One0    => a.addParam(p, n)
        case _: Const0  => a.addParam(p, n)

        case Apply1(v: V0, op: Op0) =>
          a.addEqn(p, Eqn1(p, v.analyze(a), op))

        case Fold1(v: V1, op: UnaryOp[S0, S1]) =>
          a.addEqn(p, FoldEqn1(p, v.analyze(a), op))

        case Fold1(v: V2, op: UnaryOp[S0, S2]) =>
          a.addEqn(p, FoldEqn1(p, v.analyze(a), op))

        case Apply2(l: V0, r: V0, op: Op00) =>
          a.addEqn(p, Eqn2(p, l.analyze(a), r.analyze(a), op))

        case Fold2(l: V1, r: V1, op: BinaryOp[S0, S1, S1]) =>
          a.addEqn(p, FoldEqn2(p, l.analyze(a), r.analyze(a), op))

        case Fold2(l: V2, r: V2, op: BinaryOp[S0, S2, S2]) =>
          a.addEqn(p, FoldEqn2(p, l.analyze(a), r.analyze(a), op))
      }
    }
  }

  implicit def analyze1[S <: Shape]: Analyze[S1] = new Analyze[S1] {

    def analyze(n: ValueExpr[S1], a: Analyzing): Param[S1] = {
      val p = Param[S1]()

      n match {
        case _: Var1    => a.addParam(p, n)
        case _: ArbVar1 => a.addParam(p, n)
        case _: Zero1   => a.addParam(p, n)
        case _: Half1   => a.addParam(p, n)
        case _: One1    => a.addParam(p, n)
        case _: Const1  => a.addParam(p, n)

        case Apply1(v: V1, op: Op0) =>
          a.addEqn[S1](p, Eqn1(p, v.analyze(a), op))

        case Fill(v: V0, shape: S1) =>
          a.addEqn(p, FillEqn1(p, v.analyze(a), shape))

        case Apply2(l: V1, r: V1, op: Op00) =>
          a.addEqn(p, Eqn2(p, l.analyze(a), r.analyze(a), op))

        case ElementwiseLeft(l: V1, r: V0, op: Op00) =>
          a.addEqn(p, ElementwiseEqn2(p, l.analyze(a), r.analyze(a), op))

        case ElementwiseRight(l: V0, r: V1, op: Op00) =>
          a.addEqn(p, ElementwiseEqn2(p, l.analyze(a), r.analyze(a), op))
      }
    }
  }

  implicit def analyze2[S <: Shape]: Analyze[S2] = new Analyze[S2] {

    def analyze(n: ValueExpr[S2], a: Analyzing): Param[S2] = {
      val p = Param[S2]()

      n match {
        case _: Var2    => a.addParam(p, n)
        case _: ArbVar2 => a.addParam(p, n)
        case _: Zero2   => a.addParam(p, n)
        case _: Half2   => a.addParam(p, n)
        case _: One2    => a.addParam(p, n)
        case _: Const2  => a.addParam(p, n)

        case Apply1(v: V2, op: Op0) =>
          a.addEqn[S2](p, Eqn1(p, v.analyze(a), op))

        case Fill(v: V0, shape: S2) =>
          a.addEqn(p, FillEqn1(p, v.analyze(a), shape))

        case Apply2(l: V2, r: V2, op: Op00) =>
          a.addEqn(p, Eqn2(p, l.analyze(a), r.analyze(a), op))

        case ElementwiseLeft(l: V2, r: V0, op: Op00) =>
          a.addEqn(p, ElementwiseEqn2(p, l.analyze(a), r.analyze(a), op))

        case ElementwiseRight(l: V0, r: V2, op: Op00) =>
          a.addEqn(p, ElementwiseEqn2(p, l.analyze(a), r.analyze(a), op))
      }
    }
  }

}
