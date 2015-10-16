package com.kogecoo.scalaad.test.helper.specgen

import com.kogecoo.scalaad.graph.Node
import com.kogecoo.scalaad.rule.{Value, ValueRule}

import scala.language.higherKinds


abstract class UnaryOpSpecDef[U[_], T](implicit vr: ValueRule[U, T]) {

  def op(a: Node[U, T]): Node[U, T]

  def applyExpectation(a: Node[U, T]): Value[U, T]

  def derivSelfExpectation(a: Node[U, T]): Value[U, T]

  def derivExpectation(a: Node[U, T], b: Node[U, T]): Value[U, T]

  def propagateExpectation(a: Node[U, T], b: Value[U, T]): Value[U, T]

  def gradExpectation(a: Node[U, T]): Value[U, T]


}
