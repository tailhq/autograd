package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Apply0, Apply00, V0}


case object Add00 extends Op00 { def apply(l: V0, r: V0): V0 = Apply00(l, r, Add00) }

case object Sub00 extends Op00 { def apply(l: V0, r: V0): V0 = Apply00(l, r, Sub00) }

case object Mul00 extends Op00 { def apply(l: V0, r: V0): V0 = Apply00(l, r, Mul00) }

case object Div00 extends Op00 { def apply(l: V0, r: V0): V0 = Apply00(l, r, Div00) }

case object Pos0 extends Op0 { def apply(v: V0): V0 = Apply0(v, Pos0) }

case object Neg0 extends Op0 { def apply(v: V0): V0 = Apply0(v, Neg0) }

