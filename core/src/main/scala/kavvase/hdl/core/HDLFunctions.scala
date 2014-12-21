package kavvase.hdl.core

import scalaz.{Free, Functor}
import scalaz.Free._

trait HDLFunctions {

  def not(in: Bit)(implicit F: Functor[HDL]): Free[HDL, Bit] = liftF(NOT(in, identity))

  def and(in1: Bit, in2: Bit)(implicit F: Functor[HDL]): Free[HDL, Bit] = liftF(AND(in1, in2, identity))

  def or(in1: Bit, in2: Bit)(implicit F: Functor[HDL]): Free[HDL, Bit] = liftF(OR(in1, in2, identity))

  def nand(in1: Bit, in2: Bit)(implicit F: Functor[HDL]): Free[HDL, Bit] = {
    for {
      b1 <- and(in1, in2)
      b2 <- not(b1)
    } yield b2
  }

  def nor(in1: Bit, in2: Bit)(implicit F: Functor[HDL]): Free[HDL, Bit] = {
    for {
      b1 <- or(in1, in2)
      b2 <- not(b1)
    } yield b2
  }

  def xor(in1: Bit, in2: Bit)(implicit F: Functor[HDL]): Free[HDL, Bit] = {
    for {
      b1 <- or(in1, in2)
      b2 <- and(in1, in2)
      b3 <- not(b2)
      b4 <- and(b1, b3)
    } yield b4
  }

  def xnor(in1: Bit, in2: Bit)(implicit F: Functor[HDL]): Free[HDL, Bit] = {
    for {
      b1 <- xor(in1, in2)
      b2 <- not(b1)
    } yield b2
  }

}
