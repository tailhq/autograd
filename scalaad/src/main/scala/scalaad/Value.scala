package scalaad


trait Value[A] {

  def value(t: Tensor): A

}
