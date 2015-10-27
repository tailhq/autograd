package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.value.{ContainerValue, Value}

import scala.language.higherKinds


case class ScalarConst[U[_], T](data: T)(implicit r: ValueRule[U, T]) extends Node[U, T] {
  override def toString: String = data.toString
  override def apply(): Value[U, T] = r.toValue(data)
  override def deriv(wrt: Node[U, T]): Value[U, T] = r.zero
  override def propagate(g: Value[U, T]): Value[U, T] = g * r.zero
}

case class ContainerConst[U[_], T](data: U[T])(implicit r: ValueRule[U, T]) extends Node[U, T] {
  override def toString: String = data.toString
  override def apply(): Value[U, T] = ContainerValue[U, T](data)
  override def deriv(wrt: Node[U, T]): Value[U, T] = r.zero(data)
  override def propagate(g: Value[U, T]): Value[U, T] = g * r.zero(data)
}

