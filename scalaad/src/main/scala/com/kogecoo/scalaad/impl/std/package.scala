package com.kogecoo.scalaad.impl


package object std {

  type T0 = Double
  type T1 = Vec[T0]
  type T2 = Mat[T0]

  type Vec[T] = Seq[T]
  type Mat[T] = Seq[Vec[T]]

}
