package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule

import scala.language.higherKinds


// wrapper for intermediate value of derivation.
abstract class Value[U[_], T] {
  def +(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def -(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def *(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def /(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T]

  def +(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def -(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def *(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T]
  def /(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T]

  def unary_+()(implicit vr: ValueRule[U, T]): Value[U, T]
  def unary_-()(implicit vr: ValueRule[U, T]): Value[U, T]

}

case class NonContainerValue[U[_], T](data: T) extends Value[U, T] {

  type C = ContainerValue[U, T]
  type NC = NonContainerValue[U, T]

  def +(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    rhs match {
      case r: C  => ContainerValue[U, T](vr.addMS(data, r.data))
      case r: NC => NonContainerValue[U, T](vr.addMM(data, r.data))
    }
  }

  def +(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    ContainerValue[U, T](vr.addMS(data, rhs))
  }

  def -(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    rhs match {
      case r: C  => ContainerValue[U, T](vr.subMS(data, r.data))
      case r: NC => NonContainerValue[U, T](vr.subMM(data, r.data))
    }
  }

  def -(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
     ContainerValue[U, T](vr.subMS(data, rhs))
   }

  def *(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    rhs match {
      case r: C  => ContainerValue[U, T](vr.mulMS(data, r.data))
      case r: NC => NonContainerValue[U, T](vr.mulMM(data, r.data))
    }
  }

  def *(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
     ContainerValue[U, T](vr.mulMS(data, rhs))
  }

  def /(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    rhs match {
      case r: C  => ContainerValue[U, T](vr.divMS(data, r.data))
      case r: NC => NonContainerValue[U, T](vr.divMM(data, r.data))
    }
  }

  def /(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
     ContainerValue[U, T](vr.mulMS(data, rhs))
  }

  def unary_+()(implicit vr: ValueRule[U, T]): Value[U, T] = NonContainerValue[U, T](vr.posM(data))

  def unary_-()(implicit vr: ValueRule[U, T]): Value[U, T] = NonContainerValue[U, T](vr.negM(data))

}

case class ContainerValue[U[_], T](data: U[T]) extends Value[U, T] {

  type C = ContainerValue[U, T]
  type NC = NonContainerValue[U, T]

  def +(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    rhs match {
      case r: C  => ContainerValue[U, T](vr.addSS(data, r.data))
      case r: NC => ContainerValue[U, T](vr.addSM(data, r.data))
    }
  }

  def +(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    ContainerValue[U, T](vr.addSS(data, rhs))
  }

  def -(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    rhs match {
      case r: C  => ContainerValue[U, T](vr.subSS(data, r.data))
      case r: NC => ContainerValue[U, T](vr.subSM(data, r.data))
    }
  }

  def -(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    ContainerValue[U, T](vr.subSS(data, rhs))
  }

  def *(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    rhs match {
      case r: C  => ContainerValue[U, T](vr.mulSS(data, r.data))
      case r: NC => ContainerValue[U, T](vr.mulSM(data, r.data))
    }
  }

  def *(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    ContainerValue[U, T](vr.mulSS(data, rhs))
  }

  def /(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    rhs match {
      case r: C  => ContainerValue[U, T](vr.divSS(data, r.data))
      case r: NC => ContainerValue[U, T](vr.divSM(data, r.data))
    }
  }

  def /(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
    ContainerValue[U, T](vr.divSS(data, rhs))
  }

  def unary_+()(implicit vr: ValueRule[U, T]): Value[U, T] = ContainerValue[U, T](vr.posS(data))

  def unary_-()(implicit vr: ValueRule[U, T]): Value[U, T] = ContainerValue[U, T](vr.negS(data))
}
