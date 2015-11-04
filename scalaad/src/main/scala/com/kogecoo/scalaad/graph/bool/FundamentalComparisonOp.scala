package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.graph.V


case class Eq(l: V, r: V) extends Elementwise2C(l, r)

case class Neq(l: V, r: V) extends Elementwise2C(l, r)

case class Lt(l: V, r: V) extends Elementwise2C(l, r)

case class Lte(l: V, r: V) extends Elementwise2C(l, r)

case class Gt(l: V, r: V) extends Elementwise2C(l, r)

case class Gte(l: V, r: V) extends Elementwise2C(l, r)
