package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph.bool.BooleanExpr
import shapeless.Nat
import shapeless.Nat.{_0, _1, _2}

import scala.Predef.{any2stringadd => _}


package object graph {

  type V0 = ValueExpr[_0]

  type V1 = ValueExpr[_1]

  type V2 = ValueExpr[_2]

  type V[N <: Nat] = ValueExpr[N]


  type BE0 = BooleanExpr[_0]

  type BE1 = BooleanExpr[_1]

  type BE2 = BooleanExpr[_2]

  type BE[N <: Nat] = BooleanExpr[N]
}
