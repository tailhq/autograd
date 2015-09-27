package com.kogecoo.scalaad.rule

import scala.language.higherKinds

// Define concrete calculations for U[T] type instances which performed on nodes in computational graph.
trait ValueRule[U[_], T] {

  val zeroAdd: U[T]  // T + zeroAdd() = T
  val zeroMul: U[T]  // T * zeroMul() = T
  val derivConst: U[T]

  // wrap/unwrapping value for treat as scalar
  def wrap(v: T): U[T]   // FIXME

  // because of type erasure, we cannot every 'add' methods to be a same name

  // binary ops for set and set (container and container)
  // (above 'set' doesn't means Scala's type of Set)
  def addSS(l: U[T], r: U[T]): U[T]
  def subSS(l: U[T], r: U[T]): U[T]
  def mulSS(l: U[T], r: U[T]): U[T]
  def divSS(l: U[T], r: U[T]): U[T]

  // binary ops for set and memeber (container and member)
  // here we don't assume the commutative law for add/mul.
  def addSM(l: U[T], r: T): U[T]
  def subSM(l: U[T], r: T): U[T]
  def mulSM(l: U[T], r: T): U[T]
  def divSM(l: U[T], r: T): U[T]

  // binary ops for memeber and set (member and set)
  def addMS(l: T, r: U[T]): U[T]
  def subMS(l: T, r: U[T]): U[T]
  def mulMS(l: T, r: U[T]): U[T]
  def divMS(l: T, r: U[T]): U[T]

  // binary ops for memeber and member
  def addMM(l: T, r: T): T
  def subMM(l: T, r: T): T
  def mulMM(l: T, r: T): T
  def divMM(l: T, r: T): T

  def pos(v: U[T]): U[T]
  def neg(v: U[T]): U[T]
}

trait MathRule[U[_], T] extends ValueRule[U, T] {

  def sin(v: U[T]): U[T]
  def cos(v: U[T]): U[T]
  def tan(v: U[T]): U[T]
  def ln(v: U[T]): U[T]

}
