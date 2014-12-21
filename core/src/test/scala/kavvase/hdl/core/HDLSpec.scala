package kavvase.hdl.core

import org.specs2.mutable.Specification

class HDLSpec extends Specification {

  "hdl" should {

    "support not gate" in {
      val not1 = HDL.not(Bit.High)
      val not2 = HDL.not(Bit.Low)

      HDLInterpreter.run(not1) mustEqual Bit.Low
      HDLInterpreter.run(not2) mustEqual Bit.High
    }

    "support and gate" in {
      val and1 = HDL.and(Bit.High, Bit.High)
      val and2 = HDL.and(Bit.High, Bit.Low)
      val and3 = HDL.and(Bit.Low, Bit.High)
      val and4 = HDL.and(Bit.Low, Bit.Low)

      HDLInterpreter.run(and1) mustEqual Bit.High
      HDLInterpreter.run(and2) mustEqual Bit.Low
      HDLInterpreter.run(and3) mustEqual Bit.Low
      HDLInterpreter.run(and4) mustEqual Bit.Low
    }

    "support or gate" in {
      val or1 = HDL.or(Bit.High, Bit.High)
      val or2 = HDL.or(Bit.High, Bit.Low)
      val or3 = HDL.or(Bit.Low, Bit.High)
      val or4 = HDL.or(Bit.Low, Bit.Low)

      HDLInterpreter.run(or1) mustEqual Bit.High
      HDLInterpreter.run(or2) mustEqual Bit.High
      HDLInterpreter.run(or3) mustEqual Bit.High
      HDLInterpreter.run(or4) mustEqual Bit.Low
    }

    "support nand gate" in {
      val nand1 = HDL.nand(Bit.High, Bit.High)
      val nand2 = HDL.nand(Bit.High, Bit.Low)
      val nand3 = HDL.nand(Bit.Low, Bit.High)
      val nand4 = HDL.nand(Bit.Low, Bit.Low)

      HDLInterpreter.run(nand1) mustEqual Bit.Low
      HDLInterpreter.run(nand2) mustEqual Bit.High
      HDLInterpreter.run(nand3) mustEqual Bit.High
      HDLInterpreter.run(nand4) mustEqual Bit.High
    }

    "support nor gate" in {
      val nor1 = HDL.nor(Bit.High, Bit.High)
      val nor2 = HDL.nor(Bit.High, Bit.Low)
      val nor3 = HDL.nor(Bit.Low, Bit.High)
      val nor4 = HDL.nor(Bit.Low, Bit.Low)

      HDLInterpreter.run(nor1) mustEqual Bit.Low
      HDLInterpreter.run(nor2) mustEqual Bit.Low
      HDLInterpreter.run(nor3) mustEqual Bit.Low
      HDLInterpreter.run(nor4) mustEqual Bit.High
    }

    "support xor gate" in {
      val xor1 = HDL.xor(Bit.High, Bit.High)
      val xor2 = HDL.xor(Bit.High, Bit.Low)
      val xor3 = HDL.xor(Bit.Low, Bit.High)
      val xor4 = HDL.xor(Bit.Low, Bit.Low)

      HDLInterpreter.run(xor1) mustEqual Bit.Low
      HDLInterpreter.run(xor2) mustEqual Bit.High
      HDLInterpreter.run(xor3) mustEqual Bit.High
      HDLInterpreter.run(xor4) mustEqual Bit.Low
    }

    "support xnor gate" in {
      val xnor1 = HDL.xnor(Bit.High, Bit.High)
      val xnor2 = HDL.xnor(Bit.High, Bit.Low)
      val xnor3 = HDL.xnor(Bit.Low, Bit.High)
      val xnor4 = HDL.xnor(Bit.Low, Bit.Low)

      HDLInterpreter.run(xnor1) mustEqual Bit.High
      HDLInterpreter.run(xnor2) mustEqual Bit.Low
      HDLInterpreter.run(xnor3) mustEqual Bit.Low
      HDLInterpreter.run(xnor4) mustEqual Bit.High
    }

  }

}
