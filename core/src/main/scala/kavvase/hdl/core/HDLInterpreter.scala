package kavvase.hdl.core

import scalaz.Free

object HDLInterpreter {

  def run[A](hdl: Free[HDL, A]): A = {
    hdl.go(interpret)
  }

  private def interpret[A](hdl: HDL[A]): A = {
    hdl match {
      case Not(in, out) => out(not(in))
      case And(in1, in2, out) => out(and(in1, in2))
      case Or(in1, in2, out) => out(or(in1, in2))
      case Plus(in1, in2, out) => out(plus(in1, in2))
      case Times(in1, in2, out) => out(times(in1, in2))
    }
  }

  private def not(in: Bool): Bool = Bool(!in.value)

  private def and(in1: Bool, in2: Bool): Bool = Bool(in1.value && in2.value)

  private def or(in1: Bool, in2: Bool): Bool = Bool(in1.value || in2.value)

  private def plus(in1: Signed, in2: Signed)(implicit e: Numeric[Signed]): Signed = e.plus(in1, in2)

  private def times(in1: Signed, in2: Signed)(implicit e: Numeric[Signed]): Signed = e.times(in1, in2)

}
