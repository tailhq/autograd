package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.rule.Implicits.BaseRuleOps

import scala.language.higherKinds


case class ScalarConst[U[_], T](data: T)(implicit r: ValueRule[U, T]) extends Node[U, T] {
  override def toString: String = data.toString
  override def apply(): U[T] = r.wrap(data)
  override def deriv(wrt: Node[U, T]): U[T] = r.derivConst
  override def propagate(g: U[T]): U[T] = r.derivConst * g

  def unwrap(): T = data
}

case class ContainerConst[U[_], T](data: U[T])(implicit r: ValueRule[U, T]) extends Node[U, T] {
  override def toString: String = data.toString
  override def apply(): U[T] = data
  override def deriv(wrt: Node[U, T]): U[T] = r.derivConst
  override def propagate(g: U[T]): U[T] = r.derivConst * g
}

