package kavvase.hdl.core

import org.specs2.mutable.Specification

class HDLSpec extends Specification {

  "hdl" should {

    "support not gate" in {
      val not1 = HDL.not(Bool(High))
      val not2 = HDL.not(Bool(Low))

      HDLInterpreter.run(not1) mustEqual Bool(Low)
      HDLInterpreter.run(not2) mustEqual Bool(High)
    }

    "support and gate" in {
      val and1 = HDL.and(Bool(High), Bool(High))
      val and2 = HDL.and(Bool(High), Bool(Low))
      val and3 = HDL.and(Bool(Low), Bool(High))
      val and4 = HDL.and(Bool(Low), Bool(Low))

      HDLInterpreter.run(and1) mustEqual Bool(High)
      HDLInterpreter.run(and2) mustEqual Bool(Low)
      HDLInterpreter.run(and3) mustEqual Bool(Low)
      HDLInterpreter.run(and4) mustEqual Bool(Low)
    }

    "support or gate" in {
      val or1 = HDL.or(Bool(High), Bool(High))
      val or2 = HDL.or(Bool(High), Bool(Low))
      val or3 = HDL.or(Bool(Low), Bool(High))
      val or4 = HDL.or(Bool(Low), Bool(Low))

      HDLInterpreter.run(or1) mustEqual Bool(High)
      HDLInterpreter.run(or2) mustEqual Bool(High)
      HDLInterpreter.run(or3) mustEqual Bool(High)
      HDLInterpreter.run(or4) mustEqual Bool(Low)
    }

    "support nand gate" in {
      val nand1 = HDL.nand(Bool(High), Bool(High))
      val nand2 = HDL.nand(Bool(High), Bool(Low))
      val nand3 = HDL.nand(Bool(Low), Bool(High))
      val nand4 = HDL.nand(Bool(Low), Bool(Low))

      HDLInterpreter.run(nand1) mustEqual Bool(Low)
      HDLInterpreter.run(nand2) mustEqual Bool(High)
      HDLInterpreter.run(nand3) mustEqual Bool(High)
      HDLInterpreter.run(nand4) mustEqual Bool(High)
    }

    "support nor gate" in {
      val nor1 = HDL.nor(Bool(High), Bool(High))
      val nor2 = HDL.nor(Bool(High), Bool(Low))
      val nor3 = HDL.nor(Bool(Low), Bool(High))
      val nor4 = HDL.nor(Bool(Low), Bool(Low))

      HDLInterpreter.run(nor1) mustEqual Bool(Low)
      HDLInterpreter.run(nor2) mustEqual Bool(Low)
      HDLInterpreter.run(nor3) mustEqual Bool(Low)
      HDLInterpreter.run(nor4) mustEqual Bool(High)
    }

    "support xor gate" in {
      val xor1 = HDL.xor(Bool(High), Bool(High))
      val xor2 = HDL.xor(Bool(High), Bool(Low))
      val xor3 = HDL.xor(Bool(Low), Bool(High))
      val xor4 = HDL.xor(Bool(Low), Bool(Low))

      HDLInterpreter.run(xor1) mustEqual Bool(Low)
      HDLInterpreter.run(xor2) mustEqual Bool(High)
      HDLInterpreter.run(xor3) mustEqual Bool(High)
      HDLInterpreter.run(xor4) mustEqual Bool(Low)
    }

    "support xnor gate" in {
      val xnor1 = HDL.xnor(Bool(High), Bool(High))
      val xnor2 = HDL.xnor(Bool(High), Bool(Low))
      val xnor3 = HDL.xnor(Bool(Low), Bool(High))
      val xnor4 = HDL.xnor(Bool(Low), Bool(Low))

      HDLInterpreter.run(xnor1) mustEqual Bool(High)
      HDLInterpreter.run(xnor2) mustEqual Bool(Low)
      HDLInterpreter.run(xnor3) mustEqual Bool(Low)
      HDLInterpreter.run(xnor4) mustEqual Bool(High)
    }

    "support addition of unsigned number" in {
      val signed1 = Signed(List(Low, Low, Low, Low, Low, Low, Low, High), 8)
      val signed2 = Signed(List(Low, High, High, High, High, High, High, Low), 8)
      val signed3 = Signed(List(High, High, High, High, High, High, High, Low), 8)

      val plus1 = HDL.plus(signed1, signed2)
      val plus2 = HDL.plus(signed2, signed3)
      val plus3 = HDL.plus(signed3, signed1)

      HDLInterpreter.run(plus1) mustEqual Signed(List(Low, High, High, High, High, High, High, High), 8)
      HDLInterpreter.run(plus2) mustEqual Signed(List(Low, High, High, High, High, High, Low, Low), 8)
      HDLInterpreter.run(plus3) mustEqual Signed(List(High, High, High, High, High, High, High, High), 8)
    }

    "support multiplication of unsigned number" in {
      val signed1 = Signed(List(Low, Low, Low, Low, Low, Low, High, Low), 8)
      val signed2 = Signed(List(Low, Low, Low, Low, Low, Low, High, High), 8)
      val signed3 = Signed(List(High, High, High, High, High, High, High, Low), 8)

      val times1 = HDL.times(signed1, signed2)
      val times2 = HDL.times(signed2, signed3)
      val times3 = HDL.times(signed3, signed1)

      HDLInterpreter.run(times1) mustEqual Signed(List(Low, Low, Low, Low, Low, High, High, Low), 8)
      HDLInterpreter.run(times2) mustEqual Signed(List(High, High, High, High, High, Low, High, Low), 8)
      HDLInterpreter.run(times3) mustEqual Signed(List(High, High, High, High, High, High, Low, Low), 8)
    }

  }

}
