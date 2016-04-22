package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape


trait Expr[S <: Shape] { val shape: S }
