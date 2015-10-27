package com.kogecoo.scalaad.breeze.test.helper.gen

import com.kogecoo.scalaad.graph.{ContainerConst, ScalarConst, Var, Node}
import com.kogecoo.scalaad.test.helper.gen._
import org.scalacheck.Gen
import org.scalacheck.Gen._

import scala.language.higherKinds


abstract class ShapeRestrictedNodeGen[U[_], T] {

  final def genNode(shape: MatrixShape, restrict: T => Boolean): Gen[Node[U, T]] = {
    oneOf(genScalarConst(restrict), genContainerConst(shape, restrict), genVar(shape, restrict))
  }

  final def genVar(shape: MatrixShape, restrict: T => Boolean): Gen[Var[U, T]] = {
    genVarWithSource(shape, restrict).map(_.node)
  }

  final def genScalarConst(restrict: T => Boolean): Gen[ScalarConst[U, T]] = {
    genScalarConstWithSource(restrict).map(_.node)
  }

  final def genContainerConst(shape: MatrixShape, restrict: T => Boolean): Gen[ContainerConst[U, T]] = {
    genContainerConstWithSource(shape, restrict).map(_.node)
  }

  def genScalarConstWithSource(restrict: T => Boolean): Gen[ScalarConstSample[U, T]]

  def genContainerConstWithSource(shape: MatrixShape, restrict: T => Boolean): Gen[ContainerConstSample[U, T]]

  def genVarWithSource(shape: MatrixShape, restrict: T=> Boolean): Gen[VarSample[U, T]]

}
