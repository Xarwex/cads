object PS2Q6 {

  trait Register[T] {
    def write(x: T): Unit

    def read: T
  }

  trait BooleanAtomicRegister extends Register[Boolean]

  def intToBooleanArray(i: Int): Array[Boolean] = ???

  def booleanArrayToInt(array: Array[BooleanAtomicRegister]): Int = ???

  class AcmeWriteOnceRegister(N: Int) extends Register[Int] {

    private val b = new Array[BooleanAtomicRegister](3 * N)

    def write(x: Int) = {
      val v = intToBooleanArray(x) // convert x into array of N bits
      // copy v to b[0..N), b[N..2N) and b[2N..3N), in ascending index order
      for (i <- 0 until N) b(i).write(v(i))
      for (i <- 0 until N) b(i + N).write(v(i))
      for (i <- 0 until N) b(i + 2 * N).write(v(i))
    }

    def read: Int = {
      if (booleanArrayToInt(b.slice(N + 1, 2 * N + 1)) > 0))
        return booleanArrayToInt(b.slice(0, N + 1))
      0
    }
  }
}