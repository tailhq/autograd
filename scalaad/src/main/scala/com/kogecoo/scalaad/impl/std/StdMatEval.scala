package com.kogecoo.scalaad.impl.std



trait StdMatEval {
/*
  private[this] type BOp = (T0, T0) => T0

  private[this] def map12row(l: V1, r: V2, f: BOp): T2 = r.eval[T2].map(l.eval[T1].zip(_).map { case (x, y) => f(x, y) })
  private[this] def map21row(l: V2, r: V1, f: BOp): T2 = l.eval[T2].map(_.zip(r.eval[T1]).map { case (x, y) => f(x, y) })
  private[this] def map12col(l: V1, r: V2, f: BOp): T2 = r.eval[T2].zip(l.eval[T1]).map { case (y, x) => y.map(f(_, x)) }
  private[this] def map21col(l: V2, r: V1, f: BOp): T2 = l.eval[T2].zip(r.eval[T1]).map { case (x, y) => x.map(f(_, y)) }

  private[this] def map12(l: V1, r: V2, f: BOp): T2 = if (l.shape.transposed) map12row(l, r, f) else map12col(l, r, f)
  private[this] def map21(l: V2, r: V1, f: BOp): T2 = if (r.shape.transposed) map21row(l, r, f) else map21col(l, r, f)

  implicit val eval22_stdmat_double: Eval[V2, T2] = new Eval[V2, T2] {

    def eval(n: V2): T2 = n match {

      // Leaf nodes
      case Var2(v, _)                => v.value[T2]
      case ArbVar2(sig, data, shape) => data.get.value[T2]
      case Zero2(shape)              => U.const2(0.0, shape)
      case Half2(shape)              => U.const2(0.5, shape)
      case One2(shape)               => U.const2(1.0, shape)
      case Const2(v, shape)          => v.value[T2]
      case Eye2(shape)               => U.diag(1.0, shape)

      // Unary ops
      case Pos2(v) => U.broadcast2(v.eval[T2], +_)
      case Neg2(v) => U.broadcast2(v.eval[T2], -_)
      case Transpose2(Transpose2(v)) => v.eval[T2]
      case Transpose2(v) => { // maybe too slow
        val m = v.eval[T2]
        val rows = m.shape._1
        val cols = m.shape._2
        (0 until cols).map { c =>
          (0 until rows).map { r =>
            m(r)(c)
          }
        }
      }

      // Binary ops
      case Add02(l: V0, r: V2) => U.broadcast2(r.eval[T2], l.eval[T0] + _)
      case Add20(l: V2, r: V0) => U.broadcast2(l.eval[T2], _ + r.eval[T0])
      case Add22(l: V2, r: V2) => U.elementwise2(l.eval[T2], r.eval[T2], _ + _)

      case Sub02(l: V0, r: V2) => U.broadcast2(r.eval[T2], l.eval[T0] - _)
      case Sub20(l: V2, r: V0) => U.broadcast2(l.eval[T2], _ - r.eval[T0])
      case Sub22(l: V2, r: V2) => U.elementwise2(l.eval[T2], r.eval[T2], _ - _)

      case Mul02(l: V0, r: V2) => U.broadcast2(r.eval[T2], l.eval[T0] * _)
      case Mul12(l: V1, r: V2) => map12(l, r, _ * _)
      case Mul20(l: V2, r: V0) => U.broadcast2(l.eval[T2], _ * r.eval[T0])
      case Mul21(l: V2, r: V1) => map21(l, r, _ * _)
      case Mul22(l: V2, r: V2) => U.elementwise2(l.eval[T2], r.eval[T2], _ * _)

      case Div02(l: V0, r: V2) => U.broadcast2(r.eval[T2], l.eval[T0] / _)
      case Div12(l: V1, r: V2) => map12(l, r, _ / _)
      case Div20(l: V2, r: V0) => U.broadcast2(l.eval[T2], _ / r.eval[T0])
      case Div21(l: V2, r: V1) => map21(l, r, _ / _)
      case Div22(l: V2, r: V2) => U.elementwise2(l.eval[T2], r.eval[T2], _ / _)

      // Math
      case Sin2(v) => U.broadcast2(v.eval[T2], math.sin)
      case Cos2(v) => U.broadcast2(v.eval[T2], math.cos)
      case Tan2(v) => U.broadcast2(v.eval[T2], math.tan)

      case Asin2(v) => U.broadcast2(v.eval[T2], math.asin)
      case Acos2(v) => U.broadcast2(v.eval[T2], math.acos)
      case Atan2(v) => U.broadcast2(v.eval[T2], math.atan)

      case Sinh2(v) => U.broadcast2(v.eval[T2], math.sinh)
      case Cosh2(v) => U.broadcast2(v.eval[T2], math.cosh)
      case Tanh2(v) => U.broadcast2(v.eval[T2], math.tanh)

      case Ln2(v)              => U.broadcast2(v.eval[T2], math.log)
      case Exp2(v: V2)         => U.broadcast2(v.eval[T2], math.exp)
      case Sqrt2(v: V2)        => U.broadcast2(v.eval[T2], math.sqrt)
      case Pow02(l: V0, r: V2) => U.broadcast2(r.eval[T2], math.pow(l.eval[T0], _))
      case Pow20(l: V2, r: V0) => U.broadcast2(l.eval[T2], math.pow(_, r.eval[T0]))
      case Pow22(l: V2, r: V2) => U.elementwise2(l.eval[T2], r.eval[T2], math.pow)

      // Experimental
      case Abs2(v)             => U.broadcast2(v.eval[T2], math.abs)
      case Max02(l: V0, r: V2) => U.broadcast2(r.eval[T2], math.max(l.eval[T0], _))
      case Max20(l: V2, r: V0) => U.broadcast2(l.eval[T2], math.max(_, r.eval[T0]))
      case Max22(l: V2, r: V2) => U.elementwise2(l.eval[T2], r.eval[T2], math.max)
      case Min02(l: V0, r: V2) => U.broadcast2(r.eval[T2], math.min(l.eval[T0], _))
      case Min20(l: V2, r: V0) => U.broadcast2(l.eval[T2], math.min(_, r.eval[T0]))
      case Min22(l: V2, r: V2) => U.elementwise2(l.eval[T2], r.eval[T2], math.min)

      case Where0_2(cond: B0, a: V2, b: V2) => {
        if (cond.eval[Boolean]) a.eval[StdMat[T0]] else b.eval[StdMat[T0]]
      }
      case Where1_2(cond: B1, a: V2, b: V2) => {
        val ae = a.eval[T2]
        val be = b.eval[T2]
        val ce = cond.eval[StdVec[Boolean]]
        ce.zip(ae.zip(be)).map { case (c, (x, y))  => if (c) x else y }
      }

      case Where2_2(cond: B2, a: V2, b: V2) => {
        val x = a.eval[T2]
        val y = b.eval[T2]
        cond.eval[StdMat[Boolean]].zipWithIndex.map { case (r, i) =>
          r.zipWithIndex.map { case (c, j) => if (c) x(i)(j) else y(i)(j) }
        }
      }

      case MatMul22(a: V2, b: V2) => {  //assert(a(0).size == b.size)
        val aeval = a.eval[T2]
        val beval = b.eval[T2]

        assert(aeval.head.size == beval.size)
        (0 until b.shape._2).map { bcolIndex =>
          aeval.map { arow =>
            arow.zip(beval.map(_.apply(bcolIndex))).map({ case (x, y) => x * y }).sum
          }
        }
      }
    }
  }

  // Experimental
  /*implicit val eval20_stdmat_dobule: Eval[N2, Double] = new Eval[N2, Double] {

    private[this] type T = Double
    private[this] type V = StdMat[T]

    def eval(n: N2): T = n match {
      case Max2(v: N2) => v.eval[V].map(_.max).max
      case Min2(v: N2) => v.eval[V].map(_.min).min
    }
  }*/

  implicit val eval_bool22_stdmat_double: Eval[B2, StdMat[Boolean]] = new Eval[B2, StdMat[Boolean]] {

    type BM = StdMat[Boolean]
    type B  = Boolean

    private[this] def broadcast2B(a: BM, f: B => B): BM = a.map(_.map(f))

    private[this] def elementwise2B(a: BM, b: BM, f: (B, B) => B): BM = {
      a.zip(b).map { case (x, y) => x.zip(y).map { case (v, w) => f(v, w) } }
    }

    def eval(n: B2): BM = n match {

      case Eq02(l: V0, r: V2)  => U.broadcast2(r.eval[T2], l.eval[T0] == _)
      case Eq20(l: V2, r: V0)  => U.broadcast2(l.eval[T2], r.eval[T0] == _)
      case Eq22(l: V2, r: V2)  => U.elementwise2(l.eval[T2], r.eval[T2], _ == _)

      case Neq02(l: V0, r: V2) => U.broadcast2(r.eval[T2], l.eval[T0] != _)
      case Neq20(l: V2, r: V0) => U.broadcast2(l.eval[T2], r.eval[T0] != _)
      case Neq22(l: V2, r: V2) => U.elementwise2(l.eval[T2], r.eval[T2], _ != _)

      case Lt02(l: V0, r: V2)  => U.broadcast2(r.eval[T2], l.eval[T0] < _)
      case Lt20(l: V2, r: V0)  => U.broadcast2(l.eval[T2], r.eval[T0] < _)
      case Lt22(l: V2, r: V2)  => U.elementwise2(l.eval[T2], r.eval[T2], _ <  _)

      case Lte02(l: V0, r: V2) => U.broadcast2(r.eval[T2], l.eval[T0] <= _)
      case Lte20(l: V2, r: V0) => U.broadcast2(l.eval[T2], r.eval[T0] <= _)
      case Lte22(l: V2, r: V2) => U.elementwise2(l.eval[T2], r.eval[T2], _ <= _)

      case Gt02(l: V0, r: V2)  => U.broadcast2(r.eval[T2], l.eval[T0] > _)
      case Gt20(l: V2, r: V0)  => U.broadcast2(l.eval[T2], r.eval[T0] > _)
      case Gt22(l: V2, r: V2)  => U.elementwise2(l.eval[T2], r.eval[T2], _ > _)

      case Gte02(l: V0, r: V2) => U.broadcast2(r.eval[T2], l.eval[T0] >= _)
      case Gte20(l: V2, r: V0) => U.broadcast2(l.eval[T2], r.eval[T0] >= _)
      case Gte22(l: V2, r: V2) => U.elementwise2(l.eval[T2], r.eval[T2], _ >= _)

      case And02(l: B0, r: B2) => broadcast2B(r.eval[BM], l.eval[B] && _)
      case And20(l: B2, r: B0) => broadcast2B(l.eval[BM], _ && r.eval[B])
      case And22(l: B2, r: B2) => elementwise2B(l.eval[BM], r.eval[BM], _ && _)

      case Or02(l: B0, r: B2)  => broadcast2B(r.eval[BM], l.eval[B] && _)
      case Or20(l: B2, r: B0)  => broadcast2B(l.eval[BM], _ && r.eval[B])
      case Or22(l: B2, r: B2)  => elementwise2B(l.eval[BM], r.eval[BM],_ || _)

      case Not2(v: B2) => v.eval[StdMat[Boolean]].map(_.map(!_))

    }
  }

*/
}
