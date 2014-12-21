package kavvase.hdl.core

import scalaz.{Free, Functor}
import scalaz.Free._

trait HDLFunctions {

  def not(in: Bool)(implicit F: Functor[HDL]): Free[HDL, Bool] = liftF(Not(in, identity))

  def and(in1: Bool, in2: Bool)(implicit F: Functor[HDL]): Free[HDL, Bool] = liftF(And(in1, in2, identity))

  def or(in1: Bool, in2: Bool)(implicit F: Functor[HDL]): Free[HDL, Bool] = liftF(Or(in1, in2, identity))

  def nand(in1: Bool, in2: Bool)(implicit F: Functor[HDL]): Free[HDL, Bool] = {
    for {
      b1 <- and(in1, in2)
      b2 <- not(b1)
    } yield b2
  }

  def nor(in1: Bool, in2: Bool)(implicit F: Functor[HDL]): Free[HDL, Bool] = {
    for {
      b1 <- or(in1, in2)
      b2 <- not(b1)
    } yield b2
  }

  def xor(in1: Bool, in2: Bool)(implicit F: Functor[HDL]): Free[HDL, Bool] = {
    for {
      b1 <- or(in1, in2)
      b2 <- and(in1, in2)
      b3 <- not(b2)
      b4 <- and(b1, b3)
    } yield b4
  }

  def xnor(in1: Bool, in2: Bool)(implicit F: Functor[HDL]): Free[HDL, Bool] = {
    for {
      b1 <- xor(in1, in2)
      b2 <- not(b1)
    } yield b2
  }

  def plus(in1: Signed, in2: Signed)(implicit F: Functor[HDL]): Free[HDL, Signed] = liftF(Plus(in1, in2, identity))

  def times(in1: Signed, in2: Signed)(implicit F: Functor[HDL]): Free[HDL, Signed] = liftF(Times(in1, in2, identity))

}
