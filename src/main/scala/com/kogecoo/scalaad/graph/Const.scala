package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.{ContainerValue, Value, ValueRule}

import scala.language.higherKinds


abstract class Const[U[_], T](implicit r: ValueRule[U, T]) extends Node[U, T] {
  override def deriv(wrt: Node[U, T]): Value[U, T] = r.derivConst
  override def propagate(g: Value[U, T]): Value[U, T] = g * r.derivConst
}

case class ScalarConst[U[_], T](data: T)(implicit r: ValueRule[U, T]) extends Const[U, T]()(r) {
  override def toString: String = data.toString
  override def apply(): Value[U, T] = r.toValue(data)
}

case class ContainerConst[U[_], T](data: U[T])(implicit r: ValueRule[U, T]) extends Const[U, T]()(r) {
  override def toString: String = data.toString
  override def apply(): Value[U, T] = ContainerValue[U, T](data)
}

