package kavvase.hdl.core

import scalaz.Functor

sealed trait HDL[A]

case class NOT[A](
  in: Bit,
  out: Bit => A
) extends HDL[A]

case class AND[A](
  in1: Bit,
  in2: Bit,
  out: Bit => A
) extends HDL[A]

case class OR[A](
  in1: Bit,
  in2: Bit,
  out: Bit => A
) extends HDL[A]

object HDL extends HDLInstances with HDLFunctions

trait HDLInstances {

  implicit object HDLFunctor extends Functor[HDL] {
    def map[A, B](hdl: HDL[A])(f: A => B): HDL[B] = {
      hdl match {
        case NOT(in, out)       => NOT(in, bit => f(out(bit)))
        case AND(in1, in2, out) => AND(in1, in2, bit => f(out(bit)))
        case OR(in1, in2, out)  => OR(in1, in2, bit => f(out(bit)))
      }
    }
  }

}

