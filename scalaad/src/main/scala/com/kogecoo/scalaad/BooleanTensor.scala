package com.kogecoo.scalaad

import shapeless.Nat


trait BooleanTensor[N <: Nat] {

  def shape: Shape[N]

  def value[A](implicit v: BooleanValue[N, A]): A = v.value(this)

}
