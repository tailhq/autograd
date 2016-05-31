package com.kogecoo.scalaad.analyze

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.ValueExpr

import scala.collection.GenMap


case class Analyzed(
  eqns: GenMap[Param[_ <: Shape], Equation[_ <: Shape]],
  params: GenMap[Param[_ <: Shape], ValueExpr[_ <: Shape]]
)
