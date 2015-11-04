package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape


// Nullary Application

trait Application0 extends ValueExpr


trait Apply0 extends Application0 {

  // Reference: https://en.wikipedia.org/wiki/Matrix_calculus#Vector-by-vector
  final def forwardOutputShape(wrt: ValueExpr): Shape = wrt.shape.concat(shape)

}
