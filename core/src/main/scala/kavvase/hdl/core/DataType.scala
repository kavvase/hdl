package kavvase.hdl.core

sealed trait DataType

case class Bit(value: Boolean) extends DataType

object Bit {

  val High: Bit = Bit(true)

  val Low: Bit = Bit(false)

}
