package io.github.tailabs.autograd.value

import io.github.tailabs.autograd.rule.ValueRule

import scala.language.higherKinds


// wrapper for intermediate value of derivation.
abstract class Value[U[_], T] {
  def +(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def -(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def *(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def /(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def <(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean]
  def >(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean]
  def <=(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean]
  def >=(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean]
  def ==(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean]
  def close(rhs: Value[U, T], eps: T)(implicit vr: ValueRule[U, T]): Value[U, Boolean]

  def +(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def -(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def *(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def /(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def <(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean]
  def >(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean]
  def <=(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean]
  def >=(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean]
  def ==(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean]
  def close(rhs: U[T], eps: T)(implicit vr: ValueRule[U, T]): Value[U, Boolean]

  def unary_+()(implicit vr: ValueRule[U, T]): Value[U, T]
  def unary_-()(implicit vr: ValueRule[U, T]): Value[U, T]

  def T()(implicit vr: ValueRule[U, T]): Value[U, T]
}

