package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.graph.B


case class Not(v: B) extends Elementwise1B(v)

case class And(l: B, r: B) extends Elementwise2B(l, r)

case class Or(l: B, r: B) extends Elementwise2B(l, r)
