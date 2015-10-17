package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.{ContainerValue, Value, ValueRule}

import scala.language.higherKinds


case class ScalarConst[U[_], T](data: T)(implicit r: ValueRule[U, T]) extends Node[U, T] {
  override def toString: String = data.toString
  override def apply(): Value[U, T] = r.toValue(data)
  override def deriv(wrt: Node[U, T]): Value[U, T] = r.zero(wrt())
  override def propagate(g: Value[U, T]): Value[U, T] = g * r.zero(g)
}

case class ContainerConst[U[_], T](data: U[T])(implicit r: ValueRule[U, T]) extends Node[U, T] {
  override def toString: String = data.toString
  override def apply(): Value[U, T] = ContainerValue[U, T](data)
  override def deriv(wrt: Node[U, T]): Value[U, T] = r.zero(wrt())
  override def propagate(g: Value[U, T]): Value[U, T] = g * r.zero(g)
}

