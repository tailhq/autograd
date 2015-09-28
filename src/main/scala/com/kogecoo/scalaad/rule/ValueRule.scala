package com.kogecoo.scalaad.rule

import com.kogecoo.scalaad.graph.Value

import scala.language.higherKinds


// Define concrete calculations for U[T] type instances which performed on nodes in computational graph.
trait ValueRule[U[_], T] {

  val zeroAdd: Value[U, T]  // T + zeroAdd() = T
  val zeroMul: Value[U, T]  // T * zeroMul() = T
  val derivConst: Value[U, T]

  def wrap(v: T): Value[U, T]   // FIXME

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

  def posS(v: U[T]): U[T]
  def negS(v: U[T]): U[T]

  def posM(v: T): T
  def negM(v: T): T
}

trait MathRule[U[_], T] extends ValueRule[U, T] {

  def sinS(v: U[T]): U[T]
  def cosS(v: U[T]): U[T]
  def tanS(v: U[T]): U[T]
  def lnS(v: U[T]): U[T]

  def sinM(v: T): T
  def cosM(v: T): T
  def tanM(v: T): T
  def lnM(v: T): T

}
