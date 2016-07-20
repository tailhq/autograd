package scalaad.impl.std

import scalaad.Eval
import scalaad.graph._


object StdBroadcastHelper {

  def unary1[I0, O0](v: Expr, f: I0 => O0)(implicit e: Eval[Expr, Vec[I0]]): Vec[O0] = v.eval[Vec[I0]].map(f)

  def unary2[I0, O0](v: Expr, f: I0 => O0)(implicit e: Eval[Expr, Mat[I0]]): Mat[O0] = v.eval[Mat[I0]].map(_.map(f))

  def _binary1[I, O](l: Vec[I], r: Vec[I], f: (I, I) => O): Vec[O] = {
    (l.length, r.length) match {
      case (a, b) if a * b == 0 => throw new Exception(s"cannot perform broadcasting op with $l and $r")
      case (a, b) if a == b     => l.zip(r).map { case (x, y) => f(x, y) }
      case (a, 1)               => l.map { x => f(x,      r.head) }
      case (1, b)               => r.map { y => f(l.head, y     ) }
      case _                    => throw new Exception(s"cannot perform broadcasting op with $l and $r")
    }
  }

  def binary1[I0, O0](v: Elementwise2, f: (I0, I0) => O0)(implicit e0: Eval[Expr, I0], e1: Eval[Expr, Vec[I0]]): Vec[O0] = {
    val (x, y) = (v.l.shape.order, v.r.shape.order) match {
      case (1, 0) => (v.l.eval[Vec[I0]],   toVec(v.r.eval[I0]))
      case (0, 1) => (toVec(v.l.eval[I0]), v.r.eval[Vec[I0]])
      case (1, 1) => (v.l.eval[Vec[I0]],   v.r.eval[Vec[I0]])
      case (a, b) => throw new Exception(s"cannot perform broadcasting op with ${v.l} and ${v.r}")
    }
    _binary1(x, y, f)
  }

  def binary2[I0, O0](v: Elementwise2, f: (I0, I0) => O0)(implicit e0: Eval[Expr, I0], e1: Eval[Expr, Vec[I0]], e2: Eval[Expr, Mat[I0]]): Mat[O0] = {
    val (x, y) = (v.l.shape.order, v.r.shape.order) match {
      case (2, 0) => (v.l.eval[Mat[I0]],        toMat(v.r.eval[I0]))
      case (2, 1) => (v.l.eval[Mat[I0]],        toMat(v.r.eval[Vec[I0]]))
      case (0, 2) => (toMat(v.l.eval[I0]),      v.r.eval[Mat[I0]])
      case (1, 2) => (toMat(v.l.eval[Vec[I0]]), v.r.eval[Mat[I0]])
      case (2, 2) => (v.l.eval[Mat[I0]],        v.r.eval[Mat[I0]])
      case (a, b) => throw new Exception(s"cannot perform broadcasting op with ${v.l} and ${v.r}")
    }
    _binary1[Vec[I0], Vec[O0]](x, y, (a: Vec[I0], b: Vec[I0]) => _binary1[I0, O0](a, b, f))
  }

}

