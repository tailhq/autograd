package io.github.tailabs.autograd.rule

import scala.language.higherKinds


trait ValueWrapperRule[Wrappee, Wrapper[_], T] {
  def toWrapper(data: Wrappee): Wrapper[T]
}
