package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue}

import scala.language.higherKinds



class NonContainerValueSample[U[_], T](val value: NonContainerValue[U, T], val src: T) {
  override def toString = s"NonContainerValueSample(${value}, ${src})"
}

class ContainerValueSample[U[_], T](val value: ContainerValue[U, T], val src: U[T]) {
  override def toString = s"ContainerValueSample(${value}, ${src})"
}

