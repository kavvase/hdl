package kavvase.hdl.core

import scala.annotation.tailrec

sealed trait DataType

case class Bool(value: Bit) extends DataType

case class Signed(value: List[Bit], size: Int) extends DataType

object Signed {

  implicit object SignedNumeric extends Numeric[Signed] {

    def plus(x: Signed, y: Signed): Signed = {
      assert(x.size == y.size)
      fromInt(toInt(x) + toInt(y), x.size)
    }

    def minus(x: Signed, y: Signed): Signed = {
      assert(x.size == y.size)
      fromInt(toInt(x) - toInt(y), x.size)
    }

    def times(x: Signed, y: Signed): Signed = {
      assert(x.size == y.size)
      fromInt(toInt(x) * toInt(y), x.size)
    }

    def negate(x: Signed): Signed = {
      fromInt(- toInt(x), x.size)
    }

    def fromInt(x: Int): Signed = {
      val bits = x.toBinaryString.toList.map(b => if (b == '1') true else false)
      Signed(bits, bits.size)
    }

    def fromInt(x: Int, size: Int): Signed = {
      val bits = x.toBinaryString.takeRight(size).toList.map(b => if (b == '1') true else false)
      Signed(appendElements(false, size - bits.size, bits), size)
    }

    @tailrec
    private def appendElements[A](a: A, n: Int, acc: List[A]): List[A] = {
      if (n > 0) appendElements(a, n - 1, a :: acc)
      else if (n == 0) acc
      else throw new RuntimeException
    }

    def toInt(x: Signed): Int = {
      Integer.parseInt(x.value.map(if (_) 1 else 0).mkString, 2)
    }

    def toLong(x: Signed): Long = {
      toInt(x).toLong
    }

    def toFloat(x: Signed): Float = {
      toInt(x).toFloat
    }

    def toDouble(x: Signed): Double = {
      toInt(x).toDouble
    }

    def compare(x: Signed, y: Signed): Int = {
      toInt(x).compare(toInt(y))
    }

  }

}
