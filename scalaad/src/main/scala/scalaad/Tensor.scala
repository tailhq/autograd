package scalaad


trait Tensor {

  def shape: Shape

  def value[A](implicit v: Value[A]): A = v.value(this)

}
