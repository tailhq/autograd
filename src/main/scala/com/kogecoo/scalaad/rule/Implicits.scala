package com.kogecoo.scalaad.rule

import com.kogecoo.scalaad.graph.{ ContainerValue, Node, NonContainerValue, ScalarConst, Value }

import scala.language.higherKinds


object Implicits {

  implicit class ValueOps[U[_], T](val self: U[T]) extends AnyVal {
    type C = ContainerValue[U, T]
    type NC = NonContainerValue[U, T]

    def +(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
      rhs match {
        case r: NC => ContainerValue[U, T](vr.addSM(self, r.data))
        case r: C  => ContainerValue[U, T](vr.addSS(self, r.data))
      }
    }

    def -(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
      rhs match {
        case r: NC => ContainerValue[U, T](vr.subSM(self, r.data))
        case r: C  => ContainerValue[U, T](vr.subSS(self, r.data))
      }
    }

    def *(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
      rhs match {
        case r: NC => ContainerValue[U, T](vr.mulSM(self, r.data))
        case r: C  => ContainerValue[U, T](vr.mulSS(self, r.data))
      }
    }

    def /(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
      rhs match {
        case r: NC => ContainerValue[U, T](vr.divSM(self, r.data))
        case r: C  => ContainerValue[U, T](vr.divSS(self, r.data))
      }
    }

    def unary_+()(implicit vr: ValueRule[U, T]): U[T] = vr.posS(self)
    def unary_-()(implicit vr: ValueRule[U, T]): U[T] = vr.negS(self)

  }

  def sin_#[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = {
    v match {
      case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.sinM(v.data))
      case v: ContainerValue[U, T] => ContainerValue[U, T](mr.sinS(v.data))
    }
  }

  def cos_#[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = {
    v match {
      case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.cosM(v.data))
      case v: ContainerValue[U, T] => ContainerValue[U, T](mr.cosS(v.data))
    }
  }

  def tan_#[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = {
    v match {
      case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.tanM(v.data))
      case v: ContainerValue[U, T] => ContainerValue[U, T](mr.tanS(v.data))
    }
  }

  def ln_#[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = {
    v match {
      case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.lnM(v.data))
      case v: ContainerValue[U, T] => ContainerValue[U, T](mr.lnS(v.data))
    }
  }

}
