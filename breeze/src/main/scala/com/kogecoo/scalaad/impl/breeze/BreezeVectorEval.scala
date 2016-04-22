package com.kogecoo.scalaad.impl.breeze

import breeze.linalg.{Transpose, BitVector, DenseVector}
import breeze.{ numerics => bmath }
import scala.Predef.{any2stringadd => _, _}

import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.breeze.Implicits._


trait BreezeVectorEval {

  /*
  private[this] type T = Double
  private[this] type V = DenseVector[Double]

  implicit val eval11_breeze_trans_vector_double: Eval[V1, Transpose[V]] = new Eval[V1, Transpose[V]] {

    def eval(n: V1): Transpose[V] = n match {
      case Transpose1(v) => v.eval[Transpose[V]]

    }
  }

  implicit val eval11_breeze_vector_double: Eval[V1, V] = new Eval[V1, V] {

    private[this] type N = V1

    def eval(n: N): V = n match {

      // Leaf nodes
      case Var1(v, _)       => v.value[V]
      case Zero1(shape: S1) => DenseVector.zeros(shape._1)
      case Half1(shape: S1) => DenseVector.fill(shape._1)(0.5)
      case One1(shape: S1)  => DenseVector.ones(shape._1)
      case Const1(v, shape) => v.value[V]

      // Unary ops
      case Pos1(v: N) => v.eval[V].map { a => +a }
      case Neg1(v: N) => -v.eval[V]

      // Binary ops
      case Add01(l: V0, r: N)  => r.eval[V]  + l.eval[T]  // FIXME: this assuming commutative law
      case Add10(l: N , r: V0) => l.eval[V] :+ r.eval[T]
      case Add11(l: N , r: N)  => l.eval[V] :+ r.eval[V]

      case Sub01(l: V0, r: N)  => l.eval[T] - r.eval[V]
      case Sub10(l: N , r: V0) => l.eval[V] :- r.eval[T]
      case Sub11(l: N , r: N)  => l.eval[V] :- r.eval[V]

      case Mul01(l: V0, r: N)  => l.eval[T] * r.eval[V]
      case Mul10(l: N , r: V0) => l.eval[V] :* r.eval[T]
      case Mul11(l: N , r: N)  => l.eval[V] :* r.eval[V]

      case Div01(l: V0, r: N)  => l.eval[T] / r.eval[V]
      case Div10(l: N , r: V0) => l.eval[V] :/ r.eval[T]
      case Div11(l: N , r: N)  => l.eval[V] :/ r.eval[V]

      // Math
      case Sin1(v: N) => bmath.sin(v.eval[V])
      case Cos1(v: N) => bmath.cos(v.eval[V])
      case Tan1(v: N) => bmath.tan(v.eval[V])

      case Asin1(v: N) => bmath.asin(v.eval[V])
      case Acos1(v: N) => bmath.acos(v.eval[V])
      case Atan1(v: N) => bmath.atan(v.eval[V])

      case Sinh1(v: N) => bmath.sinh(v.eval[V])
      case Cosh1(v: N) => bmath.cosh(v.eval[V])
      case Tanh1(v: N) => bmath.tanh(v.eval[V])

      case Ln1(v: N)   => bmath.log(v.eval[V])
      case Exp1(v: N)  => bmath.exp(v.eval[V])
      case Sqrt1(v: N) => bmath.sqrt(v.eval[V])

      case Pow01(l: V0, r: N)  => bmath.pow(l.eval[T], r.eval[V])
      case Pow10(l: N , r: V0) => bmath.pow(l.eval[V], r.eval[T])
      case Pow11(l: N , r: N)  => bmath.pow(l.eval[V], r.eval[V])

      case Abs1(v: N)          => bmath.abs(v.eval[V])
      case Max01(l: V0, r: N)  => val a = l.eval[T]; r.eval[V].map { breeze.linalg.max(a, _) }
      case Max10(l: N , r: V0) => breeze.linalg.max(l.eval[V], r.eval[T])
      case Max11(l: N , r: N)  => breeze.linalg.max(l.eval[V], r.eval[V])
      case Min01(l: V0, r: N)  => val a = l.eval[T]; r.eval[V].map { breeze.linalg.min(a, _) }
      case Min10(l: N , r: V0) => breeze.linalg.min(l.eval[V], r.eval[T])
      case Min11(l: N , r: N)  => breeze.linalg.min(l.eval[V], r.eval[V])
    }
  }

  implicit val eval_bool11_breeze_vecotr_double: Eval[B1, BitVector] = new Eval[B1, BitVector] {

    private[this] def and10(a: BitVector, b: Boolean): BitVector = a :& BitVector.ones(a.size)
    private[this] def or10(a: BitVector, b: Boolean): BitVector = a :| BitVector.ones(a.size)

    def eval(n: B1): BitVector = n match {
      case Eq01(l: V0, r: V1)  => r.eval[V] :== l.eval[T]
      case Eq10(l: V1, r: V0)  => l.eval[V] :== r.eval[T]
      case Eq11(l: V1, r: V1)  => l.eval[V] :== r.eval[V]
      case Neq01(l: V0, r: V1) => r.eval[V] :!= l.eval[T]
      case Neq10(l: V1, r: V0) => l.eval[V] :!= r.eval[T]
      case Neq11(l: V1, r: V1) => l.eval[V] :!= r.eval[V]
      case Lt01(l: V0, r: V1)  => r.eval[V] :>  l.eval[T]
      case Lt10(l: V1, r: V0)  => l.eval[V] :<  r.eval[T]
      case Lt11(l: V1, r: V1)  => l.eval[V] :<  r.eval[V]
      case Lte01(l: V0, r: V1) => r.eval[V] :>= l.eval[T]
      case Lte10(l: V1, r: V0) => l.eval[V] :<= r.eval[T]
      case Lte11(l: V1, r: V1) => l.eval[V] :<= r.eval[V]
      case Gt01(l: V0, r: V1)  => r.eval[V] :<  l.eval[T]
      case Gt10(l: V1, r: V0)  => l.eval[V] :>  r.eval[T]
      case Gt11(l: V1, r: V1)  => l.eval[V] :>  r.eval[V]
      case Gte01(l: V0, r: V1) => r.eval[V] :<= l.eval[T]
      case Gte10(l: V1, r: V0) => l.eval[V] :>= r.eval[T]
      case Gte11(l: V1, r: V1) => l.eval[V] :>= r.eval[V]

      case And01(l: B0, r: B1) => and10(r.eval[BitVector], l.eval[Boolean])
      case And10(l: B1, r: B0) => and10(l.eval[BitVector], r.eval[Boolean])
      case And11(l: B1, r: B1) => l.eval[BitVector] :& r.eval[BitVector]
      case Or01(l: B0, r: B1)  => or10(r.eval[BitVector], l.eval[Boolean])
      case Or10(l: B1, r: B0)  => or10(l.eval[BitVector], r.eval[Boolean])
      case Or11(l: B1, r: B1)  => l.eval[BitVector] :| r.eval[BitVector]

      case Not1(v: B1) => !v.eval[BitVector]
    }
  }
  */

}

