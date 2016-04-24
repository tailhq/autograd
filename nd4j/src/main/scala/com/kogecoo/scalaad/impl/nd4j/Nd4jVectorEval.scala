package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.nd4j.Implicits._

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.nd4s.Implicits._
import scala.Predef.{any2stringadd => _, _}


trait Nd4jVectorEval {

  private[this] type T = Double
  private[this] type V = INDArray

  implicit val eval11_nd4j_vector_double: Eval[V1, V] = new Eval[V1, V] {

    private[this] type N = V1

    def eval(n: N): V = n match {

      // Leaf nodes
      case Var1(v, _)       => v.value[V]
      case Zero1(shape: S1) => Nd4j.zeros(shape._1)
      case Half1(shape: S1) => Nd4j.create(Array.fill[T](shape._1)(0.5))
      case One1(shape: S1)  => Nd4j.ones(shape._1)
      case Const1(v, shape) => v.value[V]

      // Unary ops
      case Pos1(v: N) => v.eval[V].map { a => +a }
      case Neg1(v: N) => -v.eval[V]

      // Binary ops
      case Add01(l: V0, r: N)  => r.eval[V].add(l.eval[T])  // FIXME: this assuming commutative law
      case Add10(l: N , r: V0) => l.eval[V].add(r.eval[T])
      case Add11(l: N , r: N)  => l.eval[V].add(r.eval[V])

      case Sub01(l: V0, r: N)  => r.eval[V].rsub(l.eval[T])
      case Sub10(l: N , r: V0) => l.eval[V].sub(r.eval[T])
      case Sub11(l: N , r: N)  => l.eval[V].sub(r.eval[V])

      case Mul01(l: V0, r: N)  => r.eval[V].mul(l.eval[T])  // FIXME: this assuming commutative law
      case Mul10(l: N , r: V0) => l.eval[V].mul(r.eval[T])
      case Mul11(l: N , r: N)  => l.eval[V].mul(r.eval[V])

      case Div01(l: V0, r: N)  => r.eval[V].rdiv(l.eval[T])
      case Div10(l: N , r: V0) => l.eval[V].div(r.eval[T])
      case Div11(l: N , r: N)  => l.eval[V].div(r.eval[V])

      // Math
      // TODO: use executioner
      case Sin1(v: N) => v.eval[V].map(math.sin)
      case Cos1(v: N) => v.eval[V].map(math.cos)
      case Tan1(v: N) => v.eval[V].map(math.tan)

      case Asin1(v: N) => v.eval[V].map(math.asin)
      case Acos1(v: N) => v.eval[V].map(math.acos)
      case Atan1(v: N) => v.eval[V].map(math.atan)

      case Sinh1(v: N) => v.eval[V].map(math.sinh)
      case Cosh1(v: N) => v.eval[V].map(math.cosh)
      case Tanh1(v: N) => v.eval[V].map(math.tanh)

      case Ln1(v: N)   => v.eval[V].map(math.log)
      case Exp1(v: N)  => v.eval[V].map(math.exp)
      case Sqrt1(v: N) => v.eval[V].map(math.sqrt)

      case Pow01(l: V0, r: N)  => Nd4jUtil.pow(l.eval[T], r.eval[V])
      case Pow10(l: N , r: V0) => Nd4jUtil.pow(l.eval[V], r.eval[T])
      case Pow11(l: N , r: N)  => Nd4jUtil.pow(l.eval[V], r.eval[V])

      case Abs1(v: N)          => v.eval[V].map(math.abs)
      case Max01(l: V0, r: N)  => Nd4jUtil.max(r.eval[V], l.eval[T])
      case Max10(l: N , r: V0) => Nd4jUtil.max(l.eval[V], r.eval[T])
      case Max11(l: N , r: N)  => Nd4jUtil.max(l.eval[V], r.eval[V])
      case Min01(l: V0, r: N)  => Nd4jUtil.min(r.eval[V], l.eval[T])
      case Min10(l: N , r: V0) => Nd4jUtil.min(l.eval[V], r.eval[T])
      case Min11(l: N , r: N)  => Nd4jUtil.min(l.eval[V], r.eval[V])
    }
  }

  implicit val eval_bool11_nd4j_double: Eval[B1, V] = new Eval[B1, V] {

    def eval(n: B1): V = n match {
      case Eq01(l: V0, r: V1)  => Nd4jUtil.eq(r.eval[V], l.eval[T])
      case Eq10(l: V1, r: V0)  => Nd4jUtil.eq(l.eval[V], r.eval[T])
      case Eq11(l: V1, r: V1)  => Nd4jUtil.eq(l.eval[V], r.eval[V])
      case Neq01(l: V0, r: V1) => Nd4jUtil.neq(r.eval[V], l.eval[T])
      case Neq10(l: V1, r: V0) => Nd4jUtil.neq(l.eval[V], r.eval[T])
      case Neq11(l: V1, r: V1) => Nd4jUtil.neq(l.eval[V], r.eval[V])
      case Lt01(l: V0, r: V1)  => Nd4jUtil.gt(r.eval[V], l.eval[T])
      case Lt10(l: V1, r: V0)  => Nd4jUtil.lt(l.eval[V], r.eval[T])
      case Lt11(l: V1, r: V1)  => Nd4jUtil.lt(l.eval[V], r.eval[V])
      case Lte01(l: V0, r: V1) => Nd4jUtil.gte(r.eval[V], l.eval[T])
      case Lte10(l: V1, r: V0) => Nd4jUtil.lte(l.eval[V], r.eval[T])
      case Lte11(l: V1, r: V1) => Nd4jUtil.lte(l.eval[V], r.eval[V])
      case Gt01(l: V0, r: V1)  => Nd4jUtil.lt(r.eval[V], l.eval[T])
      case Gt10(l: V1, r: V0)  => Nd4jUtil.gt(l.eval[V], r.eval[T])
      case Gt11(l: V1, r: V1)  => Nd4jUtil.gt(l.eval[V], r.eval[V])
      case Gte01(l: V0, r: V1) => Nd4jUtil.gte(r.eval[V], l.eval[T])
      case Gte10(l: V1, r: V0) => Nd4jUtil.gte(l.eval[V], r.eval[T])
      case Gte11(l: V1, r: V1) => Nd4jUtil.gte(l.eval[V], r.eval[V])

      case And01(l: B0, r: B1) => Nd4jUtil.and(r.eval[V], l.eval[Boolean])
      case And10(l: B1, r: B0) => Nd4jUtil.and(l.eval[V], r.eval[Boolean])
      case And11(l: B1, r: B1) => Nd4jUtil.and(l.eval[V], r.eval[V])
      case Or01(l: B0, r: B1)  => Nd4jUtil.or(r.eval[V], l.eval[Boolean])
      case Or10(l: B1, r: B0)  => Nd4jUtil.or(l.eval[V], r.eval[Boolean])
      case Or11(l: B1, r: B1)  => Nd4jUtil.or(l.eval[V], r.eval[V])

      case Not1(v: B1) => v.eval[V].map { e => if (e == 0.0) 1.0 else 0.0 }
    }
  }

}

