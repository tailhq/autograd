package com.kogecoo.scalaad

import shapeless.Nat

import scala.language.higherKinds


trait Tensor[N <: Nat] {

  def shape: Shape[N]

}

