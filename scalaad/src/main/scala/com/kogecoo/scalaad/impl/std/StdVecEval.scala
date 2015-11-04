package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.impl.std.StdUtil.{T0, T1}
import com.kogecoo.scalaad.impl.std.{StdUtil => U}


trait StdVecEval {

  /*implicit val eval11_stdvec_trans_double: Eval[N1, StdTransVec[Double]] = new Eval[N1, StdTransVec[Double]] {

    def eval(n: N1): StdTransVec[Double] = n match {
      case Transpose1(v: N1) if !v.shape.transposed => v.eval[StdTransVec[Double]]
    }

  }*/

  implicit val eval11_stdvec_double: Eval[N1, T1] = new Eval[N1, T1] {

    def eval(n: N1): T1 = n match {

      // Leaf nodes
      case Var1(v, _)                 => v.value[T1]
      case ArbVar1(name, data, shape) => data.get.value[T1]
      case Zero1(shape: S1)           => U.const1(0.0, shape)
      case Half1(shape: S1)           => U.const1(0.5, shape)
      case One1(shape: S1)            => U.const1(1.0, shape)
      case Const1(v, shape)           => v.value[T1]

      // Unary ops
      case Pos1(v: N1) => U.broadcast1(v.eval[T1], +_)
      case Neg1(v: N1) => U.broadcast1(v.eval[T1], -_)
      //case Transpose1(v: N1) if v.shape.transposed => v.eval[StdTransVec[Double]].flatten

      // Binary ops
      case Add01(l: N0, r: N1) => U.broadcast1(r.eval[T1], l.eval[T0] + _)
      case Add10(l: N1, r: N0) => U.broadcast1(l.eval[T1], _ + r.eval[T0])
      case Add11(l: N1, r: N1) => U.elementwise1(l.eval[T1], r.eval[T1], _ + _)

      case Sub01(l: N0, r: N1) => U.broadcast1(r.eval[T1], l.eval[T0] - _)
      case Sub10(l: N1, r: N0) => U.broadcast1(l.eval[T1], _ - r.eval[T0])
      case Sub11(l: N1, r: N1) => U.elementwise1(l.eval[T1], r.eval[T1], _ - _)

      case Mul01(l: N0, r: N1) => U.broadcast1(r.eval[T1], l.eval[T0] * _)
      case Mul10(l: N1, r: N0) => U.broadcast1(l.eval[T1], _ * r.eval[T0])
      case Mul11(l: N1, r: N1) => U.elementwise1(l.eval[T1], r.eval[T1], _ * _)

      case Div01(l: N0, r: N1) => U.broadcast1(r.eval[T1], l.eval[T0] / _)
      case Div10(l: N1, r: N0) => U.broadcast1(l.eval[T1], _ / r.eval[T0])
      case Div11(l: N1, r: N1) => U.elementwise1(l.eval[T1], r.eval[T1], _ / _)

      // Math
      case Sin1(v: N1) => U.broadcast1(v.eval[T1], math.sin)
      case Cos1(v: N1) => U.broadcast1(v.eval[T1], math.cos)
      case Tan1(v: N1) => U.broadcast1(v.eval[T1], math.tan)

      case Asin1(v: N1) => U.broadcast1(v.eval[T1], math.asin)
      case Acos1(v: N1) => U.broadcast1(v.eval[T1], math.acos)
      case Atan1(v: N1) => U.broadcast1(v.eval[T1], math.atan)

      case Sinh1(v: N1) => U.broadcast1(v.eval[T1], math.sinh)
      case Cosh1(v: N1) => U.broadcast1(v.eval[T1], math.cosh)
      case Tanh1(v: N1) => U.broadcast1(v.eval[T1], math.tanh)

      case Ln1(v: N1)   => U.broadcast1(v.eval[T1], math.log)
      case Exp1(v: N1)  => U.broadcast1(v.eval[T1], math.exp)
      case Sqrt1(v: N1) => U.broadcast1(v.eval[T1], math.sqrt)

      case Pow01(l: N0, r: N1)  => U.broadcast1(r.eval[T1], math.pow(l.eval[T0], _))
      case Pow10(l: N1 , r: N0) => U.broadcast1(l.eval[T1], math.pow(_, r.eval[T0]))
      case Pow11(l: N1 , r: N1)  => U.elementwise1(l.eval[T1], r.eval[T1], math.pow)

      // Experimental

      case VecFill(v: N0, s: S1) => U.const1(v.eval[T0], s)

      case Abs1(v: N1)          => U.broadcast1(v.eval[T1], math.abs)
      case Max01(l: N0, r: N1)  => U.broadcast1(r.eval[T1], math.max(l.eval[T0], _))
      case Max10(l: N1 , r: N0) => U.broadcast1(l.eval[T1], math.max(_, r.eval[T0]))
      case Max11(l: N1 , r: N1)  => U.elementwise1(l.eval[T1], r.eval[T1], math.max)
      case Min01(l: N0, r: N1)  => U.broadcast1(r.eval[T1], math.min(l.eval[T0], _))
      case Min10(l: N1 , r: N0) => U.broadcast1(l.eval[T1], math.min(_, r.eval[T0]))
      case Min11(l: N1 , r: N1)  => U.elementwise1(l.eval[T1], r.eval[T1], math.min)

      case Where0_1(cond: B0, a: N1, b: N1) => if (cond.eval[Boolean]) a.eval[T1] else b.eval[T1]
      case Where1_1(cond: B1, a: N1, b: N1) => {
        val ab = a.eval[T1].zip(b.eval[T1])
        val cs = cond.eval[StdVec[Boolean]]
        cs.zip(ab).map { case (c, (x, y)) => if (c) x else y }
      }
    }
  }


  implicit val eval_bool11_stdvec_double: Eval[B1, StdVec[Boolean]] = new Eval[B1, StdVec[Boolean]] {

    type BV = StdVec[Boolean]
    type B = Boolean

    private[this] def broadcast1B(a: BV, f: B => B): BV = a.map(f)

    private[this] def elementwise1B(a: BV, b: BV, f: (B, B) => B): BV = {
      a.zip(b).map { case (x, y) => f(x, y) }
    }

    def eval(n: B1): BV = n match {

      case Eq01 (l: N0, r: N1) => U.broadcast1(r.eval[T1], l.eval[T0] == _)
      case Eq10 (l: N1, r: N0) => U.broadcast1(l.eval[T1], _ == r.eval[T0])
      case Eq11 (l: N1, r: N1) => U.elementwise1(l.eval[T1], r.eval[T1], _ == _)

      case Neq01(l: N0, r: N1) => U.broadcast1(r.eval[T1], l.eval[T0] != _)
      case Neq10(l: N1, r: N0) => U.broadcast1(l.eval[T1], _ != r.eval[T0])
      case Neq11(l: N1, r: N1) => U.elementwise1(l.eval[T1], r.eval[T1], _ != _)

      case Lt01 (l: N0, r: N1) => U.broadcast1(r.eval[T1], l.eval[T0] <  _)
      case Lt10 (l: N1, r: N0) => U.broadcast1(l.eval[T1], _ <  r.eval[T0])
      case Lt11 (l: N1, r: N1) => U.elementwise1(l.eval[T1], r.eval[T1], _ <  _)

      case Lte01(l: N0, r: N1) => U.broadcast1(r.eval[T1], l.eval[T0] <= _)
      case Lte10(l: N1, r: N0) => U.broadcast1(l.eval[T1], _ <= r.eval[T0])
      case Lte11(l: N1, r: N1) => U.elementwise1(l.eval[T1], r.eval[T1], _ <= _)

      case Gt01 (l: N0, r: N1) => U.broadcast1(r.eval[T1], l.eval[T0] >  _)
      case Gt10 (l: N1, r: N0) => U.broadcast1(l.eval[T1], _ > r.eval[T0])
      case Gt11 (l: N1, r: N1) => U.elementwise1(l.eval[T1], r.eval[T1], _ >  _)

      case Gte01(l: N0, r: N1) => U.broadcast1(r.eval[T1], l.eval[T0] >= _)
      case Gte10(l: N1, r: N0) => U.broadcast1(l.eval[T1], _ >= r.eval[T0])
      case Gte11(l: N1, r: N1) => U.elementwise1(l.eval[T1], r.eval[T1], _ >= _)

      case And01(l: B0, r: B1) => broadcast1B(r.eval[BV], l.eval[B] && _)
      case And10(l: B1, r: B0) => broadcast1B(l.eval[BV], _ && r.eval[B])
      case And11(l: B1, r: B1) => elementwise1B(l.eval[BV], r.eval[BV], _ && _)

      case Or01 (l: B0, r: B1) => broadcast1B(r.eval[BV], l.eval[B] || _)
      case Or10 (l: B1, r: B0) => broadcast1B(l.eval[BV], _ || r.eval[B])
      case Or11 (l: B1, r: B1) => elementwise1B(l.eval[BV], r.eval[BV], _ || _)

      case Not1(v: B1) => broadcast1B(v.eval[BV], !_)
    }
  }

}
