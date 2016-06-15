package com.kogecoo.scalaad

import shapeless.Nat


trait Tensor[N <: Nat] {

  def shape: Shape[N]

  def value[A](implicit v: Value[N, A]): A = v.value(this)

}

