package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.graph.bool.{Elementwise2B, Elementwise2C}


object StdBroadcastHelper {

  def _unary1[I0, O0](v: Vec[I0], f: I0 => O0): Vec[O0] = v.map(f)

  def unary1[O0](v: V, f: T0 => O0)(implicit e: Eval[V, Vec[T0]]): Vec[O0] = _unary1[T0, O0](v.eval[Vec[T0]], f)

  def unary1(v: B, f: B0 => B0)(implicit e: Eval[B, Vec[B0]]): Vec[B0] = _unary1[B0, B0](v.eval[Vec[B0]], f)


  def _unary2[I0, O0](v: Mat[I0], f: I0 => O0): Mat[O0] = v.map(_.map(f))

  def unary2[O0](v: V, f: T0 => O0)(implicit e: Eval[V, Mat[T0]]): Mat[O0] = _unary2[T0, O0](v.eval[Mat[T0]], f)

  def unary2(v: B, f: B0 => B0)(implicit e: Eval[B, Mat[B0]]): Mat[B0] = _unary2[B0, B0](v.eval[Mat[B0]], f)


  def _binary1[I, O](l: Vec[I], r: Vec[I], f: (I, I) => O): Vec[O] = {
    (l.length, r.length) match {
      case (a, b) if a * b == 0 => throw new Exception(s"cannot perform broadcasting op with $l and $r")
      case (a, b) if a == b     => l.zip(r).map { case (x, y) => f(x, y) }
      case (a, 1)               => l.map { x => f(x,      r.head) }
      case (1, b)               => r.map { y => f(l.head, y     ) }
      case _                    => throw new Exception(s"cannot perform broadcasting op with $l and $r")
    }
  }

  def binary1(v: Elementwise2, f: (T0, T0) => T0)(implicit e0: Eval[V, T0], e1: Eval[V, Vec[T0]]): Vec[T0] = {
    val (x, y) = (v.l.shape.order, v.r.shape.order) match {
      case (1, 0) => (v.l.eval[Vec[T0]],   toVec(v.r.eval[T0]))
      case (0, 1) => (toVec(v.l.eval[T0]), v.r.eval[Vec[T0]])
      case (1, 1) => (v.l.eval[Vec[T0]],   v.r.eval[Vec[T0]])
      case (a, b) => throw new Exception(s"cannot perform broadcasting op with ${v.l} and ${v.r}")
    }
    _binary1(x, y, f)
  }

  def binary1C(v: Elementwise2C, f: (T0, T0) => B0)(implicit e0: Eval[V, T0], e1: Eval[V, Vec[T0]]): Vec[B0] = {
    val (x, y) = (v.l.shape.order, v.r.shape.order) match {
      case (1, 0) => (v.l.eval[Vec[T0]],   toVec(v.r.eval[T0]))
      case (0, 1) => (toVec(v.l.eval[T0]), v.r.eval[Vec[T0]])
      case (1, 1) => (v.l.eval[Vec[T0]],   v.r.eval[Vec[T0]])
      case (a, b) => throw new Exception(s"cannot perform broadcasting op with ${v.l} and ${v.r}")
    }
    _binary1(x, y, f)
  }

  def binary1B(v: Elementwise2B, f: (B0, B0) => B0)(implicit e0: Eval[B, B0], e1: Eval[B, Vec[B0]]): Vec[B0] = {
    val (x, y) = (v.l.shape.order, v.r.shape.order) match {
      case (1, 0) => (v.l.eval[Vec[B0]],   toVec(v.r.eval[B0]))
      case (0, 1) => (toVec(v.l.eval[B0]), v.r.eval[Vec[B0]])
      case (1, 1) => (v.l.eval[Vec[B0]],   v.r.eval[Vec[B0]])
      case (a, b) => throw new Exception(s"cannot perform broadcasting op with ${v.l} and ${v.r}")
    }
    _binary1(x, y, f)
  }

  def binary2(v: Elementwise2, f: (T0, T0) => T0)(implicit e0: Eval[V, T0], e1: Eval[V, Vec[T0]], e2: Eval[V, Mat[T0]]): Mat[T0] = {
    val (x, y) = (v.l.shape.order, v.r.shape.order) match {
      case (2, 0) => (v.l.eval[Mat[T0]],        toMat(v.r.eval[T0]))
      case (2, 1) => (v.l.eval[Mat[T0]],        toMat(v.r.eval[Vec[T0]]))
      case (0, 2) => (toMat(v.l.eval[T0]),      v.r.eval[Mat[T0]])
      case (1, 2) => (toMat(v.l.eval[Vec[T0]]), v.r.eval[Mat[T0]])
      case (2, 2) => (v.l.eval[Mat[T0]],        v.r.eval[Mat[T0]])
      case (a, b) => throw new Exception(s"cannot perform broadcasting op with ${v.l} and ${v.r}")
    }
    _binary1[Vec[T0], Vec[T0]](x, y, (a: Vec[T0], b: Vec[T0]) => _binary1[T0, T0](a, b, f))
  }

  def binary2C(v: Elementwise2C, f: (T0, T0) => B0)(implicit e0: Eval[V, T0], e1: Eval[V, Vec[T0]], e2: Eval[V, Mat[T0]]): Mat[B0] = {
    val (x, y) = (v.l.shape.order, v.r.shape.order) match {
      case (2, 0) => (v.l.eval[Mat[T0]],        toMat(v.r.eval[T0]))
      case (2, 1) => (v.l.eval[Mat[T0]],        toMat(v.r.eval[Vec[T0]]))
      case (0, 2) => (toMat(v.l.eval[T0]),      v.r.eval[Mat[T0]])
      case (1, 2) => (toMat(v.l.eval[Vec[T0]]), v.r.eval[Mat[T0]])
      case (2, 2) => (v.l.eval[Mat[T0]],        v.r.eval[Mat[T0]])
      case (a, b) => throw new Exception(s"cannot perform broadcasting op with ${v.l} and ${v.r}")
    }
    _binary1[Vec[T0], Vec[B0]](x, y, (a: Vec[T0], b: Vec[T0]) => _binary1[T0, B0](a, b, f))
  }

  def binary2B(v: Elementwise2B, f: (B0, B0) => B0)(implicit e0: Eval[B, B0], e1: Eval[B, Vec[B0]], e2: Eval[B, Mat[B0]]): Mat[B0] = {
    val (x, y) = (v.l.shape.order, v.r.shape.order) match {
      case (2, 0) => (v.l.eval[Mat[B0]],        toMat(v.r.eval[B0]))
      case (2, 1) => (v.l.eval[Mat[B0]],        toMat(v.r.eval[Vec[B0]]))
      case (0, 2) => (toMat(v.l.eval[B0]),      v.r.eval[Mat[B0]])
      case (1, 2) => (toMat(v.l.eval[Vec[B0]]), v.r.eval[Mat[B0]])
      case (2, 2) => (v.l.eval[Mat[B0]],        v.r.eval[Mat[B0]])
      case (a, b) => throw new Exception(s"cannot perform broadcasting op with ${v.l} and ${v.r}")
    }
    _binary1[Vec[B0], Vec[B0]](x, y, (a: Vec[B0], b: Vec[B0]) => _binary1(a, b, f))
  }

}

