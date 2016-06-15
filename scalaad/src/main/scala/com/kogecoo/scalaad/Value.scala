package com.kogecoo.scalaad

import shapeless.Nat

import scala.language.higherKinds


trait Value[N <: Nat, A] {

  def value(t: Tensor[N]): A

}

trait BooleanValue[N <: Nat, A] {

  def value(t: BooleanTensor[N]): A

}
