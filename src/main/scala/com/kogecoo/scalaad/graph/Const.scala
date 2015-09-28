package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule

import scala.language.higherKinds


case class ScalarConst[U[_], T](data: T)(implicit r: ValueRule[U, T]) extends Node[U, T] {
  override def toString: String = data.toString
  override def apply(): Value[U, T] = r.wrap(data)
  override def deriv(wrt: Node[U, T]): Value[U, T] = r.derivConst
  override def propagate(g: Value[U, T]): Value[U, T] = g * r.derivConst
  def unwrap(): T = data
}

case class ContainerConst[U[_], T](data: U[T])(implicit r: ValueRule[U, T]) extends Node[U, T] {
  override def toString: String = data.toString
  override def apply(): Value[U, T] = ContainerValue[U, T](data)
  override def deriv(wrt: Node[U, T]): Value[U, T] = r.derivConst
  override def propagate(g: Value[U, T]): Value[U, T] = g * r.derivConst
}

