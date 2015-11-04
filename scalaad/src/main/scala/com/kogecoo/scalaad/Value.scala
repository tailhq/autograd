package com.kogecoo.scalaad


trait Value[A] {

  def value(t: Tensor): A

}

trait BooleanValue[A] {

  def value(t: BooleanTensor): A

}
