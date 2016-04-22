package com.kogecoo.scalaad.algorithm

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.op._

import scala.Predef.{any2stringadd => _}

/*
trait Forward[S, SW, SO] {

  def forward(n: VE[S], wrt: VE[SW]): VE[SO]

}

/**
  * Supported combinations of Node's tensor order (shape)
  **
  *1st:  Node order which will be differentiated
  *2nd:  Node order which differentiate with respect to
  *3rd:  Node order represents differentiated node (output node)
  *with 1st with respect to node with 2nd.
  *(shape of output Node is simply deExprined by sum of lefts)
  **
  *0 0 0
  *0 1 1
  *0 2 2
  *1 0 1
  *1 1 2
  *2 0 2
  *
  **/

object Forward {

  /*
  implicit def forward000: Forward[VE0, VE0, VE0] = new Forward[VE0, VE0, VE0] {

    private[this] type N = VE0
    private[this] type W = VE0
    private[this] type O = VE0

    def forward(n: N, wrt: W): O = n match {

      // Leaf nodes
      case _: Var0    => if (n == wrt) One0() else Zero0()
      case _: ArbVar0 => if (n == wrt) One0() else Zero0()
      case _: Zero0   => Zero0()
      case _: Half0   => Zero0()
      case _: One0    => Zero0()
      case _: Const0  => Zero0()


      case Apply1(v, op) => op match {
        case Pos => +v.forward[W, O](wrt)
        case Neg => -v.forward[W, O](wrt)

        case Sin => v.forward[W, O](wrt) *  Cos(v)
        case Cos => v.forward[W, O](wrt) * -Sin(v)
        case Tan => v.forward[W, O](wrt) * (One() + Tan(v) * Tan(v))

        case Asin => v.forward[W, O](wrt) *  (One0() / Sqrt0(One0() - (v * v)))
        case Acos => v.forward[W, O](wrt) * -(One0() / Sqrt0(One0() - (v * v)))
        case Atan => v.forward[W, O](wrt) *  (One0() / (One0() + (v * v)))

        case Sinh => v.forward[W, O](wrt) * Cosh0(v)
        case Cosh => v.forward[W, O](wrt) * Sinh0(v)
        case Tanh => v.forward[W, O](wrt) * (One0()  - Tanh0(v) * Tanh0(v))

        case Ln      => v.forward[W, O](wrt) / v
        case Exp     => v.forward[W, O](wrt) * Exp0(v)
        case Sqrt    => v.forward[W, O](wrt) * (Half0() / Sqrt0(v))

        // Experimental
        case Abs     => Where0_0(v > Zero0(), v.forward[W, O](wrt), -v.forward[W, O](wrt))
      }

      case Apply00(l, r, op) => op match {
        case Add => l.forward[W, O](wrt) + r.forward[W, O](wrt)
        case Sub => l.forward[W, O](wrt) - r.forward[W, O](wrt)
        case Mul => (l.forward[W, O](wrt) * r) + (l * r.forward[W, O](wrt))
        case Div => (l.forward[W, O](wrt) / r) - ((l * r.forward[W, O](wrt)) / (r * r))
        case Pow => {
          val lhs = l.forward[W, O](wrt) * (r * Pow00(l, r - One0()))
          val rhs = Ln0(l) * Pow00(l, r) * r.forward(wrt)
          lhs + rhs }

        // Experimental
        case Max00 => Where0_0(l > r, l.forward[W, O](wrt), r.forward[W, O](wrt))
        case Min00 => Where0_0(l < r, l.forward[W, O](wrt), r.forward[W, O](wrt))
      }

      /*case Fold11(l, r, op) => op match {
        case Dot11 => Dot11(l.forward[W, O1](wrt), r) + Dot11(l, r.forward[W, O1](wrt))
      }*/



    }
  }

  implicit def forward011: Forward[V0, V1, V1] = new Forward[V0, V1, V1] {

    private[this] type N = V0
    private[this] type W = V1
    private[this] type O = V1

    def forward(n: N, wrt: W): O = n match {

      case _: Var0    => Zero1(wrt)
      case _: ArbVar0 => Zero1(wrt)
      case _: Zero0   => Zero1(wrt)
      case _: Half0   => Zero1(wrt)
      case _: One0    => Zero1(wrt)
      case _: Const0  => Zero1(wrt)

      case Pos0 => +v.forward[W, O](wrt)
      case Neg0 => -v.forward[W, O](wrt)

      case Add00 => l.forward[W, O](wrt) + r.forward[W, O](wrt)
      case Sub00 => l.forward[W, O](wrt) - r.forward[W, O](wrt)
      case Mul00 => (l.forward[W, O](wrt) :* r) + (l :* r.forward[W, O](wrt))
      case Div00 => (l.forward[W, O](wrt) :/ r) - ((l :* r.forward[W, O](wrt)) :/ (r * r))

      case Sin0 => v.forward[W, O](wrt) :*  Cos0(v)
      case Cos0 => v.forward[W, O](wrt) :* -Sin0(v)
      case Tan0 => v.forward[W, O](wrt) :* (One0() + (Tan0(v) * Tan0(v)))

      case Asin0 => v.forward[W, O](wrt) :*  (One0() / Sqrt0(One0() - (v * v)))
      case Acos0 => v.forward[W, O](wrt) :* -(One0() / Sqrt0(One0() - (v * v)))
      case Atan0 => v.forward[W, O](wrt) :*  (One0() / (One0() + (v * v)))

      case Sinh0 => v.forward[W, O](wrt) :* Cosh0(v)
      case Cosh0 => v.forward[W, O](wrt) :* Sinh0(v)
      case Tanh0 => v.forward[W, O](wrt) :* (One0() - (Tanh0(v) * Tanh0(v)))

      case Ln0      => v.forward[W, O](wrt)  :/ v
      case Exp0     => v.forward[W, O](wrt)  :* Exp0(v)
      case Sqrt0    => v.forward[W, O](wrt)  :* (Half0() / Sqrt0(v))
      case Pow00 => (l.forward[W, O](wrt) :* (r * Pow00(l, r - One0()))) + (Ln0(l) * Pow00(l, r) :* r.forward[W, O](wrt))

      // Experimental
      case Abs0     => Where0_1(v > Zero0(), v.forward[W, O](wrt), -v.forward[W, O](wrt))
      case Max00 => Where0_1(l > r, l.forward[W, O](wrt), r.forward[W, O](wrt))
      case Min00 => Where0_1(l < r, l.forward[W, O](wrt), r.forward[W, O](wrt))

    }

  }

  /*
  implicit def forward022: Forward[V0, V2, V2] = new Forward[V0, V2, V2] {

    private[this] type N = V0
    private[this] type W = V2
    private[this] type O = V2

    def forward(n: N, wrt: W): O = n match {

      // Leaf nodes
      case _: Var0    => Zero2(wrt)
      case _: ArbVar0 => Zero2(wrt)
      case _: Zero0   => Zero2(wrt)
      case _: Half0   => Zero2(wrt)
      case _: One0    => Zero2(wrt)
      case _: Const0  => Zero2(wrt)

      case Pos0(v) => +v.forward[W, O](wrt)
      case Neg0(v) => -v.forward[W, O](wrt)

      case Add00(l, r) => l.forward[W, O](wrt) + r.forward[W, O](wrt)
      case Sub00(l, r) => l.forward[W, O](wrt) - r.forward[W, O](wrt)
      case Mul00(l, r) => (l.forward[W, O](wrt) :* r) + (l :* r.forward[W, O](wrt))
      case Div00(l, r) => (l.forward[W, O](wrt) :/ r) - ((l :* r.forward[W, O](wrt)) :/ (r * r))

      case Sin0(v) => v.forward[W, O](wrt) :*  Cos0(v)
      case Cos0(v) => v.forward[W, O](wrt) :* -Sin0(v)
      case Tan0(v) => v.forward[W, O](wrt) :* (One0() + (Tan0(v) * Tan0(v)))

      case Asin0(v) => v.forward[W, O](wrt) :*  (One0() / Sqrt0(One0() - (v * v)))
      case Acos0(v) => v.forward[W, O](wrt) :* -(One0() / Sqrt0(One0() - (v * v)))
      case Atan0(v) => v.forward[W, O](wrt) :*  (One0() / (One0() + (v * v)))

      case Sinh0(v) => v.forward[W, O](wrt) :* Cosh0(v)
      case Cosh0(v) => v.forward[W, O](wrt) :* Sinh0(v)
      case Tanh0(v) => v.forward[W, O](wrt) :* (One0() - (Tanh0(v) * Tanh0(v)))

      case Ln0(v)      => v.forward[W, O](wrt)  :/ v
      case Exp0(v)     => v.forward[W, O](wrt)  :* Exp0(v)
      case Sqrt0(v)    => v.forward[W, O](wrt)  :* (Half0() / Sqrt0(v))
      case Pow00(l, r) => (l.forward[W, O](wrt) :* (r * Pow00(l, r - One0()))) + (Ln0(l) * Pow00(l, r) :* r.forward[W, O](wrt))

      // Experimental
      case Abs0(v)     => Where0_2(v > Zero0(), v.forward[W, O](wrt), -v.forward[W, O](wrt))
      case Max00(l, r) => Where0_2(l > r, l.forward[W, O](wrt), r.forward[W, O](wrt))
      case Min00(l, r) => Where0_2(l < r, l.forward[W, O](wrt), r.forward[W, O](wrt))
    }
  }

  implicit def forward101: Forward[V1, V0, V1] = new Forward[V1, V0, V1] {

    private[this] type N = V1
    private[this] type W = V0
    private[this] type O0 = V0
    private[this] type O1 = V1
    private[this] type O2 = V2

    def forward(n: N, wrt: W): O1 = n match {

      case Var1(_, shape)       => Zero1(shape)
      case ArbVar1(_, _, shape) => Zero1(shape)

      case Zero1(shape)     => Zero1(shape)
      case Half1(shape)     => Zero1(shape)
      case One1(shape)      => Zero1(shape)
      case Const1(_, shape) => Zero1(shape)

      case Pos1(v) => +v.forward[W, O1](wrt)
      case Neg1(v) => -v.forward[W, O1](wrt)

      case Add01(l, r) => l.forward[W, O0](wrt) :+ r.forward[W, O1](wrt)
      case Add10(l, r) => l.forward[W, O1](wrt) :+ r.forward[W, O0](wrt)
      case Add11(l, r) => l.forward[W, O1](wrt)  + r.forward[W, O1](wrt)
      case Sub01(l, r) => l.forward[W, O0](wrt) :- r.forward[W, O1](wrt)
      case Sub10(l, r) => l.forward[W, O1](wrt) :- r.forward[W, O0](wrt)
      case Sub11(l, r) => l.forward[W, O1](wrt)  - r.forward[W, O1](wrt)
      case Mul01(l, r) => (l.forward[W, O0](wrt) :* r) + (l :* r.forward[W, O1](wrt))
      case Mul10(l, r) => (l.forward[W, O1](wrt) :* r) + (l :* r.forward[W, O0](wrt))
      case Mul11(l, r) => (l.forward[W, O1](wrt)  * r) + (l  * r.forward[W, O1](wrt))
      case Div01(l, r) => (l.forward[W, O0](wrt) :/ r) - ((l :* r.forward[W, O1](wrt))  / (r * r))
      case Div10(l, r) => (l.forward[W, O1](wrt) :/ r) - ((l :* r.forward[W, O0](wrt)) :/ (r * r))
      case Div11(l, r) => (l.forward[W, O1](wrt)  / r) - ((l  * r.forward[W, O1](wrt))  / (r * r))

      case Sin1(v) => v.forward[W, O1](wrt) *  Cos1(v)
      case Cos1(v) => v.forward[W, O1](wrt) * -Sin1(v)
      case Tan1(v) => v.forward[W, O1](wrt) * (One0() :+ (Tan1(v) * Tan1(v)))

      case Asin1(v) => v.forward[W, O1](wrt) *  (One0() :/ Sqrt1(One0() :- (v * v)))
      case Acos1(v) => v.forward[W, O1](wrt) * -(One0() :/ Sqrt1(One0() :- (v * v)))
      case Atan1(v) => v.forward[W, O1](wrt) *  (One0() :/ (One0() :+ (v * v)))

      case Sinh1(v) => v.forward[W, O1](wrt) * Cosh1(v)
      case Cosh1(v) => v.forward[W, O1](wrt) * Sinh1(v)
      case Tanh1(v) => v.forward[W, O1](wrt) * (One0() :- (Tanh1(v) * Tanh1(v)))

      case Ln1(v)      => v.forward[W, O1](wrt) / v
      case Exp1(v)     => v.forward[W, O1](wrt) * Exp1(v)
      case Sqrt1(v)    => v.forward[W, O1](wrt) * (Half0() :/ Sqrt1(v))
      case Pow01(l, r) => (l.forward[W, O0](wrt) :* (r  * Pow01(l, r :- One0()))) + (Ln0(l) :* Pow01(l, r)  * r.forward[W, O1](wrt))
      case Pow10(l, r) => (l.forward[W, O1](wrt)  * (r :* Pow10(l, r  - One0()))) + (Ln1(l)  * Pow10(l, r) :* r.forward[W, O0](wrt))
      case Pow11(l, r) => (l.forward[W, O1](wrt)  * (r  * Pow11(l, r :- One0()))) + (Ln1(l)  * Pow11(l, r)  * r.forward[W, O1](wrt))

      // Experimental
      case Abs1(v)     => Where1_1(v :> Zero0(), v.forward[W, O1](wrt), -v.forward[W, O1](wrt))
      case Max11(l, r) => Where1_1(l  > r, l.forward[W, O1](wrt), r.forward[W, O1](wrt))
      case Min11(l, r) => Where1_1(l  < r, l.forward[W, O1](wrt), r.forward[W, O1](wrt))

      case MatMulR12(l, r) => MatMulR12(l.forward[W, O1](wrt), r) + MatMulR12(l, r.forward[W, O2](wrt))
      case MatMul2C1(l, r) => MatMul2C1(l.forward[W, O2](wrt), r) + MatMul2C1(l, r.forward[W, O1](wrt))

      case VecFill(v, s) => VecFill(v.forward[W, O0](wrt), s)
    }
  }

  implicit def forward112: Forward[V1, V1, V2] = new Forward[V1, V1, V2] {

    private[this] type N = V1
    private[this] type W = V1
    private[this] type O1 = V1
    private[this] type O2 = V2

    def forward(n: N, wrt: W): O2 = n match {

      case v: Var1    => if (n == wrt) Eye2(Shape2(v, v)) else Zero2(Shape2(v, wrt))
      case v: ArbVar1 => if (n == wrt) Eye2(Shape2(v, v)) else Zero2(Shape2(v, wrt))
      case v: Zero1   => Zero2(Shape2(v, wrt))
      case v: Half1   => Zero2(Shape2(v, wrt))
      case v: One1    => Zero2(Shape2(v, wrt))
      case v: Const1  => Zero2(Shape2(v, wrt))

      case Pos1(v) => +v.forward[W, O2](wrt)
      case Neg1(v) => -v.forward[W, O2](wrt)

      case Add01(l, r) => l.forward[W, O1](wrt) :+ r.forward[W, O2](wrt)
      case Add10(l, r) => l.forward[W, O2](wrt) :+ r.forward[W, O1](wrt)
      case Add11(l, r) => l.forward[W, O2](wrt)  + r.forward[W, O2](wrt)
      case Sub01(l, r) => l.forward[W, O1](wrt) :- r.forward[W, O2](wrt)
      case Sub10(l, r) => l.forward[W, O2](wrt) :- r.forward[W, O1](wrt)
      case Sub11(l, r) => l.forward[W, O2](wrt)  - r.forward[W, O2](wrt)
      case Mul01(l, r) => (l.forward[W, O1](wrt)  * r) :+ (l :* r.forward[W, O2](wrt))
      case Mul10(l, r) => (l.forward[W, O2](wrt) :* r) :+ (l  * r.forward[W, O1](wrt))
      case Mul11(l, r) => (l.forward[W, O2](wrt) :* r)  + (l :* r.forward[W, O2](wrt))
      case Div01(l, r) => (l.forward[W, O1](wrt)  / r) :- ((l :* r.forward[W, O2](wrt)) :/ (r * r))
      case Div10(l, r) => (l.forward[W, O2](wrt) :/ r) :- ((l  * r.forward[W, O1](wrt)) :/ (r * r))
      case Div11(l, r) => (l.forward[W, O2](wrt) :/ r)  - ((l :* r.forward[W, O2](wrt)) :/ (r * r))

      case Sin1(v) => v.forward[W, O2](wrt) :* Cos1(v)
      case Cos1(v) => v.forward[W, O2](wrt) :* -Sin1(v)
      case Tan1(v) => v.forward[W, O2](wrt) :* (One0() :+ (Tan1(v) * Tan1(v)))

      case Asin1(v) => v.forward[W, O2](wrt) :*  (One0() :/ Sqrt1(One0() :- (v * v)))
      case Acos1(v) => v.forward[W, O2](wrt) :* -(One0() :/ Sqrt1(One0() :- (v * v)))
      case Atan1(v) => v.forward[W, O2](wrt) :*  (One0() :/ (One0() :+ (v * v)))

      case Sinh1(v) => v.forward[W, O2](wrt) :* Cosh1(v)
      case Cosh1(v) => v.forward[W, O2](wrt) :* Sinh1(v)
      case Tanh1(v) => v.forward[W, O2](wrt) :* (One0() :- (Tanh1(v) * Tanh1(v)))

      case Ln1(v)      => v.forward[W, O2](wrt) :/ v
      case Exp1(v)     => v.forward[W, O2](wrt) :* Exp1(v)
      case Sqrt1(v)    => v.forward[W, O2](wrt) :* (Half0() :/ Sqrt1(v))
      case Pow01(l, r) => (l.forward[W, O1](wrt)  * (r  * Pow01(l, r :- One0()))) :+ (Ln0(l) :* Pow01(l, r) :* r.forward[W, O2](wrt))
      case Pow10(l, r) => (l.forward[W, O2](wrt) :* (r :* Pow10(l, r  - One0()))) :+ (Ln1(l)  * Pow10(l, r)  * r.forward[W, O1](wrt))
      case Pow11(l, r) => (l.forward[W, O2](wrt) :* (r  * Pow11(l, r :- One0())))  + (Ln1(l)  * Pow11(l, r) :* r.forward[W, O2](wrt))

      // Experimental
      case Abs1(v)     => Where1_2(v :> Zero0(), v.forward[W, O2](wrt), -v.forward[W, O2](wrt))
      case Max11(l, r) => Where1_2(l  > r, l.forward[W, O2](wrt), r.forward[W, O2](wrt))
      case Min11(l, r) => Where1_2(l  < r, l.forward[W, O2](wrt), r.forward[W, O2](wrt))
    }
  }

  implicit def forward202: Forward[V2, V0, V2] = new Forward[V2, V0, V2] {

    private[this] type N = V2
    private[this] type W = V0
    private[this] type O0 = V0
    private[this] type O1 = V1
    private[this] type O2 = V2

    def forward(n: N, wrt: W): O2 = n match {

      case v: Var2    => Zero2(v)
      case v: ArbVar2 => Zero2(v)
      case v: Zero2   => Zero2(v)
      case v: Half2   => Zero2(v)
      case v: One2    => Zero2(v)
      case v: Const2  => Zero2(v)

      case Pos2(v) => +v.forward[W, O2](wrt)
      case Neg2(v) => -v.forward[W, O2](wrt)

      case Add02(l, r) => l.forward[W, O0](wrt) :+ r.forward[W, O2](wrt)
      case Add20(l, r) => l.forward[W, O2](wrt) :+ r.forward[W, O0](wrt)
      case Add22(l, r) => l.forward[W, O2](wrt)  + r.forward[W, O2](wrt)
      case Sub02(l, r) => l.forward[W, O0](wrt) :- r.forward[W, O2](wrt)
      case Sub20(l, r) => l.forward[W, O2](wrt) :- r.forward[W, O0](wrt)
      case Sub22(l, r) => l.forward[W, O2](wrt)  - r.forward[W, O2](wrt)
      case Mul02(l, r) => (l.forward[W, O0](wrt) :* r) + (l :* r.forward[W, O2](wrt))
      case Mul20(l, r) => (l.forward[W, O2](wrt) :* r) + (l :* r.forward[W, O0](wrt))
      case Mul22(l, r) => (l.forward[W, O2](wrt)  * r) + (l  * r.forward[W, O2](wrt))
      case Div02(l, r) => (l.forward[W, O0](wrt) :/ r) - ((l :* r.forward[W, O2](wrt))  / (r * r))
      case Div20(l, r) => (l.forward[W, O2](wrt) :/ r) - ((l :* r.forward[W, O0](wrt)) :/ (r * r))
      case Div22(l, r) => (l.forward[W, O2](wrt)  / r) - ((l  * r.forward[W, O2](wrt))  / (r * r))

      case Sin2(v) => v.forward[W, O2](wrt) * Cos2(v)
      case Cos2(v) => v.forward[W, O2](wrt) * -Sin2(v)
      case Tan2(v) => v.forward[W, O2](wrt) * (One0() :+ (Tan2(v) * Tan2(v)))

      case Asin2(v) => v.forward[W, O2](wrt) *  (One0() :/ Sqrt2(One0() :- (v * v)))
      case Acos2(v) => v.forward[W, O2](wrt) * -(One0() :/ Sqrt2(One0() :- (v * v)))
      case Atan2(v) => v.forward[W, O2](wrt) *  (One0() :/ (One0() :+ (v * v)))

      case Sinh2(v) => v.forward[W, O2](wrt) * Cosh2(v)
      case Cosh2(v) => v.forward[W, O2](wrt) * Sinh2(v)
      case Tanh2(v) => v.forward[W, O2](wrt) * (One0() :- (Tanh2(v) * Tanh2(v)))

      case Ln2(v)      => v.forward[W, O2](wrt) / v
      case Exp2(v)     => v.forward[W, O2](wrt) * Exp2(v)
      case Sqrt2(v)    => v.forward[W, O2](wrt) * (Half0() :/ Sqrt2(v))
      case Pow02(l, r) => (l.forward[W, O0](wrt) :* (r  * Pow02(l, r :- One0()))) + (Ln0(l) :* Pow02(l, r)  * r.forward[W, O2](wrt))
      case Pow20(l, r) => (l.forward[W, O2](wrt)  * (r :* Pow20(l, r  - One0()))) + (Ln2(l)  * Pow20(l, r) :* r.forward[W, O0](wrt))
      case Pow22(l, r) => (l.forward[W, O2](wrt)  * (r  * Pow22(l, r :- One0()))) + (Ln2(l)  * Pow22(l, r)  * r.forward[W, O2](wrt))

      case Abs2(v)     => Where2_2(v :> Zero0(), v.forward[W, O2](wrt), -v.forward[W, O2](wrt))
      case Max22(l, r) => Where2_2(l  > r, l.forward[W, O2](wrt), r.forward[W, O2](wrt))
      case Min22(l, r) => Where2_2(l  < r, l.forward[W, O2](wrt), r.forward[W, O2](wrt))

      // Experimental
      case MatMul22(l, r) => MatMul22(l.forward[W, O2](wrt), r) + MatMul22(l, r.forward[W, O2](wrt))
      case MatMulC12(l, r) => MatMulC12(l.forward[W, O1](wrt), r) + MatMulC12(l, r.forward[W, O2](wrt))
      case MatMul2R1(l, r) => MatMul2R1(l.forward[W, O2](wrt), r) + MatMul2R1(l, r.forward[W, O1](wrt))
    }
  }
*/
*/
}

*/
