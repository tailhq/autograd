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

  implicit def analyze0: Analyze[S0] = new Analyze[S0] {

    def analyze(n: ValueExpr[S0], analyzing: Analyzing): Param[S0] = {
      val p = Param[S0]()

      n match {
        case _: Var0    => analyzing.addParam(p, n)
        case _: ArbVar0 => analyzing.addParam(p, n)
        case _: Zero0   => analyzing.addParam(p, n)
        case _: Half0   => analyzing.addParam(p, n)
        case _: One0    => analyzing.addParam(p, n)
        case _: Const0  => analyzing.addParam(p, n)

        case Apply1(v: V0, op: Op0) =>
          eqnProjBuilder.addEqn(p, Eqn1[S0](p, v.analyze, op))

        case Fold1(v: V1, op: UnaryOp[S0, S1]) =>
          eqnProjBuilder.addEqn(p, FoldEqn1[S0](p, v.analyze, op))

        case Fold1(v: V2, op: UnaryOp[S0, S2]) =>
          eqnProjBuilder.addEqn(p, FoldEqn1[S0](p, v.analyze, op))

        case Apply2(l: V0, r: V0, op: Op0) =>
          eqnProjBuilder.addEqn(p, Eqn2[S0](p, l.analyze, r.analyze, op))

        case Fold2(l: V1, r: V1, op: BinaryOp[S0, S1, S1]) =>
          eqnProjBuilder.addEqn(p, FoldEqn2[S0](p, l.analyze, r.analyze, op))

        case Fold2(l: V2, r: V2, op: BinaryOp[S0, S2, S2]) =>
          eqnProjBuilder.addEqn(p, FoldEqn2[S0](p, l.analyze, r.analyze, op))
      }
    }
  }

  implicit def analyze1: Analyze[S1] = new Analyze[S1] {

    def analyze(n: ValueExpr[S1], analyzing: Analyzing): Param[S1] = {
      val p = Param[S1]()

      n match {
        case _: Var1    => analyzing.addParam(p, n)
        case _: ArbVar1 => analyzing.addParam(p, n)
        case _: Zero1   => analyzing.addParam(p, n)
        case _: Half1   => analyzing.addParam(p, n)
        case _: One1    => analyzing.addParam(p, n)
        case _: Const1  => analyzing.addParam(p, n)

        case Apply1(v: V1, op: Op0) =>
          analyzing.addEqn(p, Eqn1[S1](p, v.analyze, op))

        case Fold1(v: V2, op: UnaryOp[S1, S2]) =>
          analyzing.addEqn(p, FoldEqn1[S1, S2](p, v.analyze, op))

        case Fill(v: V0, op: UnaryOp[S1, S0]) =>
          analyzing.addEqn(p, Fill1Eqn[S1, S0](p, v.analyze, op))

        case Apply2(l: V1, r: V1, op: Op0) =>
          analyzing.addEqn(p, Eqn2[S1](p, l.analyze, r.analyze, op))

        case ElementwiseLeft(l: V1, r: V0, op: Op00) =>
          analyzing.addEqn(p, FoldEqn2[S1](p, l.analyze, r.analyze, op))

        case ElementwiseRight(l: V0, r: V1, op: Op00) =>
          analyzing.addEqn(p, FoldEqn2[S1](p, l.analyze, r.analyze, op))

        // case Fold2(l: V2, r: V2, op: BinaryOp[S1, S2, S2]) =>
        //   analyzing.addEqn(p, FoldEqn2[S1](p, l.compile, r.compile, op))
      }
    }
  }

  implicit def analyze2: Analyze[S2] = new Analyze[S2] {

    def analyze(n: ValueExpr[S2], analyzing: Analyzing): Param[S2] = {
      val p = Param[S2]()

      n match {
        case _: Var2    => analyzing.addParam(p, n)
        case _: ArbVar2 => analyzing.addParam(p, n)
        case _: Zero2   => analyzing.addParam(p, n)
        case _: Half2   => analyzing.addParam(p, n)
        case _: One2    => analyzing.addParam(p, n)
        case _: Const2  => analyzing.addParam(p, n)

        case Apply1(v: V2, op: Op0) =>
          analyzing.addEqn(p, Eqn1[S2](p, v.analyze, op))

        case Fill(v: V0, op: UnaryOp[S2, S0]) =>
          analyzing.addEqn(p, Fill1Eqn[S2, S0](p, v.analyze, op))

        case Apply2(l: V2, r: V2, op: Op0) =>
          analyzing.addEqn(p, Eqn2[S2](p, l.analyze, r.analyze, op))

        case ElementwiseLeft(l: V2, r: V0, op: Op00) =>
          analyzing.addEqn(p, FoldEqn2[S2](p, l.analyze, r.analyze, op))

        case ElementwiseRight(l: V0, r: V2, op: Op00) =>
          analyzing.addEqn(p, FoldEqn2[S2](p, l.analyze, r.analyze, op))
      }
    }
  }
}
