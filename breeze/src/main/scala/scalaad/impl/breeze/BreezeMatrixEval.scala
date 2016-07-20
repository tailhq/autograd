package scalaad.impl.breeze

import breeze.linalg.{*, DenseMatrix, DenseVector}
import breeze.{linalg, numerics}

import scalaad.graph._
import scalaad.{Eval, NotImplementedYet}


trait BreezeMatrixEval { self: BreezeValue =>

  // FIXME: Broadcasting
  implicit val eval_breeze_matrix_double: Eval[Expr[Real], T2] = new Eval[Expr[Real], T2] {

    def eval(n: Expr[Real]): T2 = n.shape.order match {
      case 2 => n match {
        // Nullary op
        case a: Var                    => a.data.value[T2]
        case a: Const[Real] @unchecked => a.data.value[T2]
        case a: Diag                   => a.diagVec.value[T2]
        case _: Eye            => DenseMatrix.eye(n.shape.at(0))

        // Unary op
        case Sum1(v, axis) => {
          val x = v.eval[T3]
          axis match {
            case 0 => x.foldLeft(DenseMatrix.zeros[T0](v.shape.at(1), v.shape.at(2)))(_ :+ _)
            case 1 => DenseMatrix(x.map(y => linalg.sum(y(*, ::)).toArray).toArray:_*)
            case 2 => DenseMatrix(x.map(y => linalg.sum(y(::, *)).inner.toDenseVector.toArray).toArray: _*)
          }
        }
        case Max1(v, axis) => {
          val x = v.eval[T3]
          axis match {
            case 0 => x.foldLeft(DenseMatrix.fill(v.shape.at(1), v.shape.at(2))(Double.MinValue))((a: T2, b: T2) => linalg.max(a, b))
            case 1 => DenseMatrix(x.map(y => linalg.max(y(*, ::)).toArray).toArray: _*)
            case 2 => DenseMatrix(x.map(y => linalg.max(y(::, *)).inner.toDenseVector.toArray).toArray: _*)
          }
        }
        case Min1(v, axis) => {
          val x = v.eval[T3]
          axis match {
            case 0 => x.foldLeft(DenseMatrix.fill(v.shape.at(1), v.shape.at(2))(Double.MaxValue))((a: T2, b: T2) => linalg.min(a, b))
            case 1 => DenseMatrix(x.map(y => linalg.min(y(*, ::)).toArray).toArray: _*)
            case 2 => DenseMatrix(x.map(y => linalg.min(y(::, *)).inner.toDenseVector.toArray).toArray: _*)
          }
        }

        // Binary op
        case Dot(l, r)    => throw new NotImplementedYet()
        case MatMul(l, r) => l.eval[T2] * r.eval[T2]

        // Ternary op
        case ElementwiseWhere(cond, a, b) => {
          val z = cond.eval[B2]
          val x = a.eval[T2]
          val y = b.eval[T2]
          z.mapPairs { case (k, v) => if (v) x(k) else y(k) }
        }

        // Elementwise
        case Zero(_) => DenseMatrix.zeros(n.shape.at(0), n.shape.at(1))
        case Half(_) => DenseMatrix.fill(n.shape.at(0), n.shape.at(1))(0.5)
        case One (_) => DenseMatrix.ones(n.shape.at(0), n.shape.at(1))

        case Pos(v)      => v.eval[T2]
        case Neg(v)      => -v.eval[T2]
        case Identity(v) => v.eval[T2]
        case Sign(v)     => numerics.signum(v.eval[T2])

        case Sin(v) => numerics.sin(v.eval[T2])
        case Cos(v) => numerics.cos(v.eval[T2])
        case Tan(v) => numerics.tan(v.eval[T2])

        case Asin(v) => numerics.asin(v.eval[T2])
        case Acos(v) => numerics.acos(v.eval[T2])
        case Atan(v) => numerics.atan(v.eval[T2])

        case Sinh(v) => numerics.sinh(v.eval[T2])
        case Cosh(v) => numerics.cosh(v.eval[T2])
        case Tanh(v) => numerics.tanh(v.eval[T2])

        case Ln(v)   => numerics.log(v.eval[T2])
        case Exp(v)  => numerics.exp(v.eval[T2])
        case Sqrt(v) => numerics.sqrt(v.eval[T2])

        case Abs(v)  => numerics.abs(v.eval[T2])

        // Binary op
        case Add(l, r) => l.eval[T2] :+ r.eval[T2]
        case Sub(l, r) => l.eval[T2] :- r.eval[T2]
        case Mul(l, r) => l.eval[T2] :* r.eval[T2]
        case Div(l, r) => l.eval[T2] :/ r.eval[T2]

        case Pow (l, r) => numerics.pow(l.eval[T2], r.eval[T2])

        case Max2(l, r) => {
          val x = r.eval[T2]
          l.eval[T2].mapPairs { case (k, v) => math.max(v, x(k)) }
        }
        case Min2(l, r) => {
          val x = r.eval[T2]
          l.eval[T2].mapPairs { case (k, v) => math.min(v, x(k)) }
        }

      }
    }
  }

  implicit val eval_breeze_matrix_bool: Eval[Expr[Bool], B2] = new Eval[Expr[Bool], B2] {

    def eval(n: Expr[Bool]): B2 = n.shape.order match {
      case 2 => n match {
        case Eq (l, r) => l.eval[T2] :== r.eval[T2]
        case Neq(l, r) => l.eval[T2] :!= r.eval[T2]
        case Lt (l, r) => l.eval[T2] :<  r.eval[T2]
        case Lte(l, r) => l.eval[T2] :<= r.eval[T2]
        case Gt (l, r) => l.eval[T2] :>  r.eval[T2]
        case Gte(l, r) => l.eval[T2] :>= r.eval[T2]

        case Not(v)    => !v.eval[B2]
        case And(l, r) => l.eval[B2] :& r.eval[B2]
        case Or(l, r)  => l.eval[B2] :| r.eval[B2]
      }
    }
  }

  // FIXME: generalize
  implicit val eval_breeze_tensor3_double: Eval[Expr[Real], T3] = new Eval[Expr[Real], T3] {

    private[this] def fillVector(size: Int, m: T2): T3 = DenseVector.fill(size)(m)

    private[this] def elementwise3(v: Expr[Real], f: T2 => T2): T3 = v.eval[T3].map(f)

    private[this] def elementwise3(l: Expr[Real], r: Expr[Real], f: (T2, T2) => T2): T3 = {
      val x = l.eval[T3]
      val y = r.eval[T3]
      x.mapPairs { (k, v) => f(v, y(k)) }
    }

    private[this] def elementwise3(l: Expr[Real], r: Expr[Real], f: (T0, T0) => T0)(implicit d: DummyImplicit): T3 = {
      val x = l.eval[T3]
      val y = r.eval[T3]
      x.mapPairs { (k1, v1) =>
        v1.mapPairs { (k2, v2) =>
          f(v2, y(k1)(k2))
        }
      }
    }

    def eval(n: Expr[Real]): T3 = n.shape.order match {
      case 3 => n match {
        // Nullary op
        case a: Var                    => a.data.value[T3]
        case a: Const[Real] @unchecked => a.data.value[T3]
        case a: Diag                   => throw new NotImplementedYet()
        case _: Eye                    => throw new NotImplementedYet()

        // Unary op
        case Sum1(v, axis) => throw new NotImplementedYet()
        case Max1(v, axis) => throw new NotImplementedYet()
        case Min1(v, axis) => throw new NotImplementedYet()

        // Binary op
        case Dot(l, r)    => throw new NotImplementedYet()
        case MatMul(l, r) => throw new NotImplementedYet()

        // Ternary op
        case ElementwiseWhere(cond, a, b) => {
          val z = cond.eval[B3]
          val x = a.eval[T3]
          val y = b.eval[T3]
          z.mapPairs { case (k1, v1) =>
            v1.mapPairs { case (k2, v2) =>
              if (v2) x(k1)(k2) else y(k1)(k2)
            }
          }
        }

        // Elementwise
        case Zero(_) => fillVector(n.shape.at(0), DenseMatrix.zeros(n.shape.at(1), n.shape.at(2)))
        case Half(_) => fillVector(n.shape.at(0), DenseMatrix.fill(n.shape.at(1), n.shape.at(2))(0.5))
        case One (_) => fillVector(n.shape.at(0), DenseMatrix.ones(n.shape.at(1), n.shape.at(2)))

        case Pos(v)      => v.eval[T3]
        case Neg(v)      => elementwise3(v, -_)
        case Identity(v) => v.eval[T3]
        case Sign(v)     => elementwise3(v, (a: T2) => numerics.signum(a))

        case Sin(v) => elementwise3(v, (a: T2) => numerics.sin(a))
        case Cos(v) => elementwise3(v, (a: T2) => numerics.cos(a))
        case Tan(v) => elementwise3(v, (a: T2) => numerics.tan(a))

        case Asin(v) => elementwise3(v, (a: T2) => numerics.asin(a))
        case Acos(v) => elementwise3(v, (a: T2) => numerics.acos(a))
        case Atan(v) => elementwise3(v, (a: T2) => numerics.atan(a))

        case Sinh(v) => elementwise3(v, (a: T2) => numerics.sinh(a))
        case Cosh(v) => elementwise3(v, (a: T2) => numerics.cosh(a))
        case Tanh(v) => elementwise3(v, (a: T2) => numerics.tanh(a))

        case Ln(v)   => elementwise3(v, (a: T2) => numerics.log(a))
        case Exp(v)  => elementwise3(v, (a: T2) => numerics.exp(a))
        case Sqrt(v) => elementwise3(v, (a: T2) => numerics.sqrt(a))

        case Abs(v)  => elementwise3(v, (a: T2) => numerics.abs(a))

        // Binary op
        case Add(l, r) => elementwise3(l, r, (_: T2) :+ (_: T2))
        case Sub(l, r) => elementwise3(l, r, (_: T2) :- (_: T2))
        case Mul(l, r) => elementwise3(l, r, (_: T2) :* (_: T2))
        case Div(l, r) => elementwise3(l, r, (_: T2) :/ (_: T2))

        case Pow (l, r) => elementwise3(l, r, (x: T2, y: T2) => numerics.pow(x, y))

        case Max2(l, r) => elementwise3(l, r, (x: T0, y: T0) => math.max(x, y))
        case Min2(l, r) => elementwise3(l, r, (x: T0, y: T0) => math.min(x, y))
      }
    }
  }

  implicit val eval_breeze_tensor3_bool: Eval[Expr[Bool], B3] = new Eval[Expr[Bool], B3] {

    private[this] def elementwise3C(l: Expr[Real], r: Expr[Real], f: (T2, T2) => B2): B3 = {
      val x = l.eval[T3]
      val y = r.eval[T3]
      x.mapPairs { (k, v) => f(v, y(k)) }
    }

    private[this] def elementwise3B(l: Expr[Bool], r: Expr[Bool], f: (B2, B2) => B2): B3 = {
      val x = l.eval[B3]
      val y = r.eval[B3]
      x.mapPairs { (k, v) => f(v, y(k)) }
    }

    def eval(n: Expr[Bool]): B3 = n.shape.order match {
      case 3 => n match {
        case Eq (l, r) => elementwise3C(l, r, (_: T2) :== (_: T2))
        case Neq(l, r) => elementwise3C(l, r, (_: T2) :!= (_: T2))
        case Lt (l, r) => elementwise3C(l, r, (_: T2) :<  (_: T2))
        case Lte(l, r) => elementwise3C(l, r, (_: T2) :<= (_: T2))
        case Gt (l, r) => elementwise3C(l, r, (_: T2) :>  (_: T2))
        case Gte(l, r) => elementwise3C(l, r, (_: T2) :>= (_: T2))

        case Not(v)    => v.eval[B3].map(!_)
        case And(l, r) => elementwise3B(l, r, (_: B2) :& (_: B2))
        case Or(l, r)  => elementwise3B(l, r, (_: B2) :| (_: B2))
      }
    }
  }
}
