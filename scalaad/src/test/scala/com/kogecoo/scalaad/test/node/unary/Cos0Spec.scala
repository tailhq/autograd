package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{Cos0, N0}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdCos0Spec extends Properties("Cos0") with Cos0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N0): T0 = math.cos(a.toT0)

  override def deriv(a: T0): T0 = -math.sin(a)

}

trait Cos0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  override def op(a: N0): N0 = Cos0(a)

  override def op(argStr: String): String = s"cos($argStr)"

}


