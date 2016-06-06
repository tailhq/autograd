package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import shapeless.Nat


trait Expr[N <: Nat] { def shape: Shape[N] }
