package kavvase.hdl.core

import scalaz.Free

object HDLInterpreter {

  def run[A](hdl: Free[HDL, A]): A = {
    hdl.go(interpret)
  }

  private def interpret[A](hdl: HDL[A]): A = {
    hdl match {
      case NOT(in, out)       => out(not(in))
      case AND(in1, in2, out) => out(and(in1, in2))
      case OR(in1, in2, out)  => out(or(in1, in2))
    }
  }

  private def not(in: Bit): Bit = Bit(!in.value)

  private def and(in1: Bit, in2: Bit): Bit = Bit(in1.value && in2.value)

  private def or(in1: Bit, in2: Bit): Bit = Bit(in1.value || in2.value)

}
