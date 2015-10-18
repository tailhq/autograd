package com.kogecoo.scalaad.test.helper.specgen

import com.kogecoo.scalaad.graph.Node
import com.kogecoo.scalaad.rule.{ValueRule, Value}

import scala.language.higherKinds


abstract class BinaryOpSpecDef[U[_], T](implicit vr: ValueRule[U, T]) {

  def op(a: Node[U, T], b: Node[U, T]): Node[U, T]

  def applyExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T]

  def derivExpectation(a: Node[U, T], b: Node[U, T], c: Node[U, T]): Value[U, T]

  def derivWrtLeftExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T]

  def derivWrtRightExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T]

  def derivWrtSelfExpectation(a: Node[U, T]): Value[U, T]

  def propagateExpectation(a: Node[U, T], b: Node[U, T], c: Value[U, T]): Value[U, T]

  def gradExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T]

}
