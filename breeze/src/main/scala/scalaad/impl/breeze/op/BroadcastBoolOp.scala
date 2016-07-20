package scalaad.impl.breeze.op

import breeze.linalg.{BitVector, DenseVector}

import scalaad.graph.Bool
import scalaad.impl.breeze.{B0, B1, B2, B3, B4}


trait BroadcastBoolOp extends BroadcastOpBase[Bool, B0, B1, B2, B3, B4, B0, B1, B2, B3, B4] {

  def higher1(a: B0): B1 = BitVector(a)

  def higher2(a: B1): B2 = a.toDenseVector.toDenseMatrix

  def higher3(a: B2): B3 = DenseVector(a)

  def higher4(a: B3): B4 = a.toDenseMatrix


  def broadcast111(a: B1, b: B1): B1 =
    BroadcastHelper.perform_B1_B1_B1(a, b, baseOpImpl101, baseOpImpl111, exchangable)

  def broadcast222(a: B2, b: B2): B2 =
    BroadcastHelper.perform_2_2_2[B0, B0](a, b, baseOpImpl202, baseOpImpl222, exchangable)

  def broadcast333(a: B3, b: B3): B3 =
    BroadcastHelper.perform_3_3_3[B0, B0](a, b, baseOpImpl202, baseOpImpl222, exchangable)

}
