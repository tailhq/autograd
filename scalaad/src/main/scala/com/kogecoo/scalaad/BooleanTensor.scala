package com.kogecoo.scalaad


trait BooleanTensor {

  def shape: Shape

  def value[A](implicit v: BooleanValue[A]): A = v.value(this)

}
