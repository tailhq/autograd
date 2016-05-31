package com.kogecoo.scalaad.impl


package object std {

  type Std0 = Double
  type Std1 = Seq[Std0]
  type Std2 = Seq[Std1]

  type StdVec[T]      = Seq[T]
  type StdMat[T]      = Seq[Seq[T]]

}
