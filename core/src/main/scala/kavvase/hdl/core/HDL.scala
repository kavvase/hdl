package kavvase.hdl.core

import scalaz.Functor

sealed trait HDL[A]

case class Not[A](
  in: Bool,
  out: Bool => A
) extends HDL[A]

case class And[A](
  in1: Bool,
  in2: Bool,
  out: Bool => A
) extends HDL[A]

case class Or[A](
  in1: Bool,
  in2: Bool,
  out: Bool => A
) extends HDL[A]

case class Plus[A](
  in1: Signed,
  in2: Signed,
  out: Signed => A
) extends HDL[A]

case class Times[A](
  in1: Signed,
  in2: Signed,
  out: Signed => A
) extends HDL[A]

object HDL extends HDLInstances with HDLFunctions

trait HDLInstances {

  implicit object HDLFunctor extends Functor[HDL] {
    def map[A, B](hdl: HDL[A])(f: A => B): HDL[B] = {
      hdl match {
        case Not(in, out) => Not(in, bit => f(out(bit)))
        case And(in1, in2, out) => And(in1, in2, bit => f(out(bit)))
        case Or(in1, in2, out) => Or(in1, in2, bit => f(out(bit)))
        case Plus(in1, in2, out) => Plus(in1, in2, bits => f(out(bits)))
        case Times(in1, in2, out) => Times(in1, in2, bits => f(out(bits)))
      }
    }
  }

}

