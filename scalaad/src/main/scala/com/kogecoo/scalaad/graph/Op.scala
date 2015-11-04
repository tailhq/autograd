package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape2


case class Apply0(v: N0, op: Op0)

case class Elementwise1(v: N1, op: Op0)
case class Elementwise2(v: N2, op: Op0)

case class Broadcast1(v: N1, op: Op0)
case class Broadcast2(v: N1, op: Op0)

case class Elementwise11(l: N1, r: N1, op: Op00) extends Op11
case class Elementwise22(l: N2, r: N2, op: Op00) extends Op22

case class Broadcast01(l: N0, r: N1, op: Op00) extends Op02
case class Broadcast02(l: N0, r: N2, op: Op00) extends Op02

case class Rowwise12(l: N1, r: N2, op: Op00) extends Op12
case class Columnwise12(l: N1, r: N2, op: Op00) extends Op12


case class Add00(l :N0, r: N0) extends Op00
case class Sub00(l :N0, r: N0) extends Op00
case class Mul00(l :N0, r: N0) extends Op00
case class Div00(l :N0, r: N0) extends Op00

// Element-wise Pos
case class Pos0(v: N0) extends Op0
case class Neg0(v: N0) extends Op0

// Transpose
case class Transpose1(v: N1) extends Op1
case class Transpose2(v: N2) extends Op2

// Experimental
case class VecFill(override val v: N0, override val shape: S1) extends UnaryOp[S1, S0]
case class MatFill(override val v: N0, override val shape: S2) extends UnaryOp[S2, S0]

case class MatFillAcrossRow(override val v: N1, columnSize: Int) extends UnaryOp[S2, S1] {
  override val shape: S2 = Shape2(v.shape._1, columnSize)
}

case class MatFillAcrossColumn(override val v: N1, rowSize: Int) extends UnaryOp[S2, S1] {
  override val shape: S2 = Shape2(rowSize, v.shape._1)
}

