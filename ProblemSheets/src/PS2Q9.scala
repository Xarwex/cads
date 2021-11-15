import ox.cads.util.ThreadID

object PS2Q9 {

  trait Consensus[T] {
    def decide(value: T): T
  }

  abstract class BinaryConsensus extends Consensus[Boolean]

  def intToBooleanArray(i: Int): Array[Boolean] = ???
  def booleanArrayToInt(array: Array[Boolean]) : Int = ???

  class IntegerConsensus extends Consensus[Int] {
    val proposed: Array[Int] = ???
    val BCOArray: Array[BinaryConsensus] = ???

    def decide(value: Int): Int = {
      val me = ThreadID.get
      proposed(me) = value
      val threadIdBinary: Array[Boolean] = intToBooleanArray(me)
      val winnerThreadBinary: Array[Boolean] = new Array[Boolean](BCOArray.length)
      for (i <- BCOArray.indices)
        winnerThreadBinary(i) = BCOArray(i).decide(threadIdBinary(i))

      proposed(booleanArrayToInt(winnerThreadBinary))
    }
  }
}
