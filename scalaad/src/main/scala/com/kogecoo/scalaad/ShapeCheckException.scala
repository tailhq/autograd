package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph.ValueExpr
import com.kogecoo.scalaad.op.Op


class ShapeCheckException(a: ValueExpr[_], b: ValueExpr[_], op: String)
  extends Exception(
    s"$op cannot applicable for variables with shape pair ${a.shape} and ${b.shape}"
  )
