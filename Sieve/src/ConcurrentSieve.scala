import ox.cads.locks.Lock
import ox.cads.util.ThreadUtil

import java.util
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicIntegerArray}

object ConcurrentSieve {

  // I have used abomination whiles for fun :)
  val numPrimes = 1000000 // Number of primes to be generated
  val numWorkers = 16 /// Number of workers

  val candidate = new AtomicInteger(2)
  val primes = new AtomicIntegerArray(numPrimes)
  val current = new AtomicIntegerArray(numWorkers)
  val primesGenerated = new AtomicInteger(0)
  val completed = new AtomicInteger(0)

  def insertPrime(v: Int): Unit = {

    // Use compare and set to check if the value is smaller :))
    var value = v
    var index = primesGenerated.incrementAndGet() - 1
    synchronized {
      while ( {
        if (index > numPrimes - 1 || index == 0) false
        else {
          val p = primes.get(index - 1)
          p > value || p == 0
        }
      }) index -= 1

      while (index < numPrimes && value != 0) {
        value = primes.getAndSet(index, value)
        index += 1
      }
    }
  }

  def isPrime(value: Int): Boolean = {
    var index = 0
    while (index < numPrimes) {
      val p = primes.get(index).toLong
      if (p * p > value || p == 0) return true
      if (value % p == 0) return false
      index += 1
    }
    false
  }

  def worker(me: Int): Unit = {
    while (primesGenerated.get() < numPrimes) {

      val myCandidate = candidate.getAndIncrement()
      current.set(me, myCandidate)

      var canProceed = false
      while (!canProceed) {
        canProceed = true
        for (i <- 0 until numWorkers) {
          val c = current.get(i).toLong
          canProceed &&= (c * c > myCandidate || c == 0)
        }
      }

      if (isPrime(myCandidate))
        insertPrime(myCandidate)

      current.set(me, 0)
    }
    completed.getAndIncrement()
    if (me == 0) {
      while (completed.get() != numWorkers) {}
//      for (i <- 0 until numPrimes)
//        System.out.println((i + 1) + ": " + primes.get(i))
            System.out.println(numPrimes + ": " + primes.get(numPrimes - 1))
    }
  }

  def main(args: Array[String]): Unit = {
    val t0 = java.lang.System.currentTimeMillis()
    ThreadUtil.runIndexedSystem(numWorkers, worker)
    println("Time taken: " + (java.lang.System.currentTimeMillis() - t0))
  }
}
