package io.github.tailabs.autograd.graph

import io.github.tailabs.autograd.rule.ValueRule
import io.github.tailabs.autograd.value.{NonContainerValue, ContainerValue, Value}

import scala.language.higherKinds


case class ScalarConst[U[_], T](data: T)(implicit r: ValueRule[U, T]) extends Node[U, T] {
  override def toString: String = data.toString
  override def apply(): Value[U, T] = r.toValue(data)
  override def deriv(wrt: Var[U, T]): Value[U, T] = wrt() match {
    case v: NonContainerValue[U, T] => r.zero
    case v: ContainerValue[U, T]    => r.zero(v)
  }
  override def propagate(g: Value[U, T]): Value[U, T] = g * r.zero
}

case class ContainerConst[U[_], T](data: U[T])(implicit r: ValueRule[U, T]) extends Node[U, T] {
  override def toString: String = data.toString
  override def apply(): Value[U, T] = r.toValue(data)
  override def deriv(wrt: Var[U, T]): Value[U, T] = wrt() match {
    case v: NonContainerValue[U, T] => r.zero(data)
    case v: ContainerValue[U, T]    => r.zero(v)
  }
  override def propagate(g: Value[U, T]): Value[U, T] = g * r.zero(data)
}

