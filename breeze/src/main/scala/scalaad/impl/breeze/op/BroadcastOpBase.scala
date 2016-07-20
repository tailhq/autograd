package scalaad.impl.breeze.op

import breeze.generic.{MappingUFunc, UFunc}

import scalaad.graph.{DType, Expr}
import scalaad.{Eval, NotImplementedYet}


trait BroadcastOpBase[D <: DType, A0, A1, A2, A3, A4, R0, R1, R2, R3, R4] extends UFunc with MappingUFunc {

  @throws[NotImplementedYet]
  def apply[R](l: Expr[D], r: Expr[D])(
    implicit e0: Eval[Expr[D], A0],
      e1: Eval[Expr[D], A1],
      e2: Eval[Expr[D], A2],
      e3: Eval[Expr[D], A3]
  ): R = {

    val result = (l.shape.order, r.shape.order) match {
      case (0, 0) => this(l.eval[A0], r.eval[A0])
      case (0, 1) => this(l.eval[A0], r.eval[A1])
      case (0, 2) => this(l.eval[A0], r.eval[A2])
      case (0, 3) => this(l.eval[A0], r.eval[A3])

      case (1, 0) => this(l.eval[A1], r.eval[A0])
      case (1, 1) => this(l.eval[A1], r.eval[A1])
      case (1, 2) => this(l.eval[A1], r.eval[A2])
      case (1, 3) => this(l.eval[A1], r.eval[A3])

      case (2, 0) => this(l.eval[A2], r.eval[A0])
      case (2, 1) => this(l.eval[A2], r.eval[A1])
      case (2, 2) => this(l.eval[A2], r.eval[A2])
      case (2, 3) => this(l.eval[A2], r.eval[A3])

      case (3, 0) => this(l.eval[A3], r.eval[A0])
      case (3, 1) => this(l.eval[A3], r.eval[A1])
      case (3, 2) => this(l.eval[A3], r.eval[A2])
      case (3, 3) => this(l.eval[A3], r.eval[A3])

      case _ => throw new NotImplementedYet()
    }

    //FIXME:
    result.asInstanceOf[R]
  }


  def exchangable: Boolean

  // every ops assume that a and b have the same shape.

  def higher1(a: A0): A1

  def higher2(a: A1): A2

  def higher3(a: A2): A3

  def higher4(a: A3): A4


  def baseOpImpl000(a: A0, b: A0): R0

  def baseOpImpl101(a: A1, b: A0): R1

  def baseOpImpl111(a: A1, b: A1): R1

  def baseOpImpl202(a: A2, b: A0): R2

  def baseOpImpl222(a: A2, b: A2): R2


  def broadcast111(a: A1, b: A1): R1

  def broadcast222(a: A2, b: A2): R2

  def broadcast333(a: A3, b: A3): R3


  implicit def f_0_0_0: Impl2[A0, A0, R0] = new Impl2[A0, A0, R0] {
    def apply(a: A0, b: A0): R0 = baseOpImpl000(a, b)
  }

  implicit def f_0_1_1: Impl2[A0, A1, R1] = new Impl2[A0, A1, R1] {
    def apply(a: A0, b: A1): R1 = broadcast111(higher1(a), b)
  }

  implicit def f_0_2_2: Impl2[A0, A2, R2] = new Impl2[A0, A2, R2] {
    def apply(a: A0, b: A2): R2 = broadcast222(higher2(higher1(a)), b)
  }

  implicit def f_0_3_3: Impl2[A0, A3, R3] = new Impl2[A0, A3, R3] {
    def apply(a: A0, b: A3): R3 = broadcast333(higher3(higher2(higher1(a))), b)
  }

  implicit def f_1_0_1: Impl2[A1, A0, R1] = new Impl2[A1, A0, R1] {
    def apply(a: A1, b: A0): R1 = broadcast111(a, higher1(b))
  }

  implicit def f_1_1_1: Impl2[A1, A1, R1] = new Impl2[A1, A1, R1] {
    def apply(a: A1, b: A1): R1 = broadcast111(a, b)
  }

  implicit def f_1_2_2: Impl2[A1, A2, R2] = new Impl2[A1, A2, R2] {
    def apply(a: A1, b: A2): R2 = broadcast222(higher2(a), b)
  }

  implicit def f_1_3_3: Impl2[A1, A3, R3] = new Impl2[A1, A3, R3] {
    def apply(a: A1, b: A3): R3 = broadcast333(higher3(higher2(a)), b)
  }

  implicit def f_2_0_2: Impl2[A2, A0, R2] = new Impl2[A2, A0, R2] {
    def apply(a: A2, b: A0): R2 = broadcast222(a, higher2(higher1(b)))
  }

  implicit def f_2_1_2: Impl2[A2, A1, R2] = new Impl2[A2, A1, R2] {
    def apply(a: A2, b: A1): R2 = broadcast222(a, higher2(b))
  }

  implicit def f_2_2_2: Impl2[A2, A2, R2] = new Impl2[A2, A2, R2] {
    def apply(a: A2, b: A2): R2 = broadcast222(a, b)
  }

  implicit def f_2_3_3: Impl2[A2, A3, R3] = new Impl2[A2, A3, R3] {
    def apply(a: A2, b: A3): R3 = broadcast333(higher3(a), b)
  }

  implicit def f_3_0_3: Impl2[A3, A0, R3] = new Impl2[A3, A0, R3] {
    def apply(a: A3, b: A0): R3 = broadcast333(a, higher3(higher2(higher1(b))))
  }

  implicit def f_3_1_3: Impl2[A3, A1, R3] = new Impl2[A3, A1, R3] {
    def apply(a: A3, b: A1): R3 = broadcast333(a, higher3(higher2(b)))
  }

  implicit def f_3_2_3: Impl2[A3, A2, R3] = new Impl2[A3, A2, R3] {
    def apply(a: A3, b: A2): R3 = broadcast333(a, higher3(b))
  }

  implicit def f_3_3_3: Impl2[A3, A3, R3] = new Impl2[A3, A3, R3] {
    def apply(a: A3, b: A3): R3 = broadcast333(a, b)
  }

}

