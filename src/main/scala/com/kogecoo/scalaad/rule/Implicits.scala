package com.kogecoo.scalaad.rule

import scala.language.higherKinds


object Implicits {

  //  BaseRuleOps and BaseRuleOps are aliases of methods of ValueRule.
  // These are used for easy/intuitive writing of calculations performed in
  // nodes of computational tree.
  implicit class BaseRuleOps[U[_], T](val self: U[T]) extends AnyVal {

    def +(rhs: U[T])(implicit r: ValueRule[U, T]): U[T] = r.addSS(self, rhs)
    def -(rhs: U[T])(implicit r: ValueRule[U, T]): U[T] = r.subSS(self, rhs)
    def *(rhs: U[T])(implicit r: ValueRule[U, T]): U[T] = r.mulSS(self, rhs)
    def /(rhs: U[T])(implicit r: ValueRule[U, T]): U[T] = r.divSS(self, rhs)

    def +>(rhs: T)(implicit r: ValueRule[U, T]): U[T] = r.addSM(self, rhs)
    def ->(rhs: T)(implicit r: ValueRule[U, T]): U[T] = r.subSM(self, rhs)
    def *>(rhs: T)(implicit r: ValueRule[U, T]): U[T] = r.mulSM(self, rhs)
    def />(rhs: T)(implicit r: ValueRule[U, T]): U[T] = r.divSM(self, rhs)

    def unary_+()(implicit r: ValueRule[U, T]): U[T] = r.pos(self)
    def unary_-()(implicit r: ValueRule[U, T]): U[T] = r.neg(self)
  }

  implicit class BaseRuleOps2[U[_], T](val self: T) extends AnyVal {

    def +<(rhs: U[T])(implicit r: ValueRule[U, T]): U[T] = r.addMS(self, rhs)
    def -<(rhs: U[T])(implicit r: ValueRule[U, T]): U[T] = r.subMS(self, rhs)
    def *<(rhs: U[T])(implicit r: ValueRule[U, T]): U[T] = r.mulMS(self, rhs)
    def /<(rhs: U[T])(implicit r: ValueRule[U, T]): U[T] = r.divMS(self, rhs)

    def +:+(rhs: T)(implicit r: ValueRule[U, T]): T = r.addMM(self, rhs)
    def -:-(rhs: T)(implicit r: ValueRule[U, T]): T = r.subMM(self, rhs)
    def *:*(rhs: T)(implicit r: ValueRule[U, T]): T = r.mulMM(self, rhs)
    def /:/(rhs: T)(implicit r: ValueRule[U, T]): T = r.divMM(self, rhs)
  }

  implicit class MathRuleOps[U[_], T](val self: U[T]) extends AnyVal {

    def sin(v: U[T])(implicit r: MathRule[U, T]): U[T] = r.sin(self)
    def cos(v: U[T])(implicit r: MathRule[U, T]): U[T] = r.cos(self)
    def tan(v: U[T])(implicit r: MathRule[U, T]): U[T] = r.tan(self)
    def ln(v: U[T])(implicit r: MathRule[U, T]): U[T] = r.ln(self)

  }
}
