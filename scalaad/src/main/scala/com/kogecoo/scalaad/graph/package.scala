package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph.bool.BooleanExpr

import Predef.{any2stringadd => _}


package object graph {

  type VE0 = ValueExpr[S0]

  type VE1 = ValueExpr[S1]

  type VE2 = ValueExpr[S2]

  type VE[S <: Shape] = ValueExpr[S]


  type BE0 = BooleanExpr[S0]

  type BE1 = BooleanExpr[S1]

  type BE2 = BooleanExpr[S2]

  type BE[S <: Shape] = BooleanExpr[S]
}
