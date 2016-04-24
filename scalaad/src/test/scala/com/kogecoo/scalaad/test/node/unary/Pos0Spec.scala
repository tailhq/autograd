package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{V0, Pos0}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdPos0Spec extends Properties("Pos0") with Pos0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V0): T0 = +a.toT0

  override def deriv(a: T0): T0 =  1.0

}


trait Pos0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  override def op(a: V0): V0 = Pos0(a)

  override def op(argStr: String): String = s"+$argStr"

}

