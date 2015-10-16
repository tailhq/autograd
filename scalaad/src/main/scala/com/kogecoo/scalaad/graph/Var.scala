package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.{Value, ValueRule, ValueWrapperRule}

import scala.language.higherKinds


class Var[U[_], T](val data: U[T])(implicit r: ValueRule[U, T]) extends Node[U, T] {
  var gradient: Value[U, T] = r.zero(data)

  override def toString: String = s"Var[${ data }]"
  override def apply(): Value[U, T] = r.toValue(data)
  override def deriv(wrt: Node[U, T]): Value[U, T] = {
    if (wrt == this) {
      r.one(data)
    } else {
      r.zero(data)
    }
  }

  override def propagate(g: Value[U, T]): Value[U, T] = { gradient += g * r.one(g); g }
}

object Var {
  def apply[U[_], T](data: U[T])(implicit r: ValueRule[U, T]): Var[U, T] = new Var[U, T](data)
  def apply[Src, U[_], T](data: Src)(implicit r: ValueRule[U, T], f: ValueWrapperRule[Src, U, T]): Var[U, T] = {
    new Var[U, T](f.toWrapper(data))
  }
}
