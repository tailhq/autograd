package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.graph.{Node, ContainerConst, Var, ScalarConst}
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

import scala.language.higherKinds


abstract class GenNode[U[_], T] {

  lazy val defaultRestriction = (_: T) => true

  final def genNode(restrict: T => Boolean = defaultRestriction): Gen[Node[U, T]] = {
    oneOf(genScalarConst(restrict), genContainerConst(restrict), genVar(restrict))
  }

  final def genVar(restrict: T => Boolean = defaultRestriction): Gen[Var[U, T]] = {
    genVarWithSource(restrict).map(_._1)
  }

  final def genScalarConst(restrict: T => Boolean = defaultRestriction): Gen[ScalarConst[U, T]] = {
    genScalarConstWithSource(restrict).map(_._1)
  }

  final def genContainerConst(restrict: T => Boolean = defaultRestriction): Gen[ContainerConst[U, T]] = {
    genContainerConstWithSource(restrict).map(_._1)
  }

  def genVarWithSource(restrict: T => Boolean = defaultRestriction): Gen[(Var[U, T], U[T])]

  def genScalarConstWithSource(restrict: T => Boolean = defaultRestriction): Gen[(ScalarConst[U, T], T)]

  def genContainerConstWithSource(restrict: T => Boolean = defaultRestriction): Gen[(ContainerConst[U, T], U[T])]

}
