package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.graph.{Node, ContainerConst, Var, ScalarConst}
import org.scalacheck.Gen

import scala.language.higherKinds


abstract class GenNode[U[_], T] {

  lazy val defaultRestriction = (_: T) => true

  def genNode(restrict: T => Boolean = defaultRestriction): Gen[Node[U, T]]

  def genVar(restrict: T => Boolean = defaultRestriction): Gen[Var[U, T]]

  def genScalarConst(restrict: T => Boolean = defaultRestriction): Gen[ScalarConst[U, T]]

  def genContainerConst(restrict: T => Boolean = defaultRestriction): Gen[ContainerConst[U, T]]

}
