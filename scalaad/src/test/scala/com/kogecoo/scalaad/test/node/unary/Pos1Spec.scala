package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{V1, Pos1}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdPos1Spec extends Properties("Pos1") with Pos1Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V1): T1 = broadcast1(a.toT1, +_)

  override def deriv(a: T0): T0 = +1.0

}

trait Pos1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: V1): V1 = Pos1(a)

  override def op(argStr: String): String = s"+$argStr"

}


