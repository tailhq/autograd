package com.kogecoo.scalaad.rule

import scala.language.higherKinds


// Define concrete calculations for U[T] type instances which performed on nodes in computational graph.
trait ValueRule[U[_], T] {

  final def zero: Value[U, T] = toValue(zeroM)
  final def zero(shape: U[T]): Value[U, T] = toValue(zeroS(shape))
  final def zero(shape: Value[U, T]): Value[U, T] = shape match {
    case _: NonContainerValue[U, T] => zero
    case s: ContainerValue[U, T]    => zero(shape)
  }

  final def one(implicit d: DummyImplicit): Value[U, T] = toValue(oneM)
  final def one(shape: U[T])(implicit d: DummyImplicit): Value[U, T] = toValue(oneS(shape))
  final def one(shape: Value[U, T]): Value[U, T] = shape match {
    case _: NonContainerValue[U, T] => one
    case s: ContainerValue[U, T]    => one(shape)
  }

  def zeroM: T
  def zeroS(shape: U[T]): U[T]
  def oneM: T
  def oneS(shape: U[T]): U[T]

  def derivConst(shape: U[T]): Value[U, T]
  def derivConst(shape: Value[U, T]): Value[U, T]

  def toValue(v: T): Value[U, T]   // FIXME
  def toValue(v: U[T])(implicit e: DummyImplicit): Value[U, T]   // FIXME

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

  def ltSS (l: U[T], r: U[T]): U[Boolean]
  def lteSS(l: U[T], r: U[T]): U[Boolean]
  def gtSS (l: U[T], r: U[T]): U[Boolean]
  def gteSS(l: U[T], r: U[T]): U[Boolean]
  def eqSS (l: U[T], r: U[T]): U[Boolean]

  def ltSM (l: U[T], r: T): U[Boolean]
  def lteSM(l: U[T], r: T): U[Boolean]
  def gtSM (l: U[T], r: T): U[Boolean]
  def gteSM(l: U[T], r: T): U[Boolean]
  def eqSM (l: U[T], r: T): U[Boolean]

  def ltMS (l: T, r: U[T]): U[Boolean]
  def lteMS(l: T, r: U[T]): U[Boolean]
  def gtMS (l: T, r: U[T]): U[Boolean]
  def gteMS(l: T, r: U[T]): U[Boolean]
  def eqMS (l: T, r: U[T]): U[Boolean]

  def ltMM (l: T, r: T): Boolean
  def lteMM(l: T, r: T): Boolean
  def gtMM (l: T, r: T): Boolean
  def gteMM(l: T, r: T): Boolean
  def eqMM (l: T, r: T): Boolean

  def posS(v: U[T]): U[T]
  def negS(v: U[T]): U[T]

  def posM(v: T): T
  def negM(v: T): T

  def transposeS(v: U[T]): U[T]
  def transposeM(v: T): T

  def whereSSS(cond: U[Boolean], a: U[T], b: U[T]): U[T]
  def whereSSM(cond: U[Boolean], a: U[T], b: T):    U[T]
  def whereSMS(cond: U[Boolean], a: T,    b: U[T]): U[T]
  def whereSMM(cond: U[Boolean], a: T,    b: T):    U[T]
  def whereMSS(cond: Boolean,    a: U[T], b: U[T]): U[T]
  def whereMSM(cond: Boolean,    a: U[T], b: T):    U[T]
  def whereMMS(cond: Boolean,    a: T,    b: U[T]): U[T]
  def whereMMM(cond: Boolean,    a: T,    b: T):    T

}

trait MathRule[U[_], T] extends ValueRule[U, T] {

  def sinS(v: U[T]): U[T]
  def cosS(v: U[T]): U[T]
  def tanS(v: U[T]): U[T]
  def asinS(v: U[T]): U[T]
  def acosS(v: U[T]): U[T]
  def atanS(v: U[T]): U[T]
  def sinhS(v: U[T]): U[T]
  def coshS(v: U[T]): U[T]
  def tanhS(v: U[T]): U[T]
  def lnS(v: U[T]): U[T]
  def expS(v: U[T]): U[T]
  def absS(v: U[T]): U[T]
  def sqrtS(v: U[T]): U[T]

  def sinM(v: T): T
  def cosM(v: T): T
  def tanM(v: T): T
  def asinM(v: T): T
  def acosM(v: T): T
  def atanM(v: T): T
  def sinhM(v: T): T
  def coshM(v: T): T
  def tanhM(v: T): T
  def lnM(v: T): T
  def expM(v: T): T
  def absM(v: T): T
  def sqrtM(v: T): T

  def powSS(v: U[T], p: U[T]): U[T]
  def powSM(v: U[T], p: T): U[T]
  def powMS(v: T, p: U[T]): U[T]
  def powMM(v: T, p: T): T

  def dotSS(v: U[T], p: U[T]): U[T]
  def dotSM(v: U[T], p: T): U[T]
  def dotMS(v: T, p: U[T]): U[T]
  def dotMM(v: T, p: T): T
}

trait ValueWrapperRule[Wrappee, Wrapper[_], T] {
  def toWrapper(data: Wrappee): Wrapper[T]
}
