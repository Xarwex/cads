import ox.cads.locks.Lock
import ox.cads.util.ThreadUtil

import java.util
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicIntegerArray}

object ConcurrentSieve {

  // I have used abomination whiles for fun :)
  val numPrimes = 1000 // Number of primes to be generated
  val numWorkers = 10 /// Number of workers

  val candidate = new AtomicInteger(2)
  val primes = new AtomicIntegerArray(numPrimes)
  val current = new AtomicIntegerArray(numWorkers)
  val primesGenerated = new AtomicInteger(0)
  val completed = new AtomicInteger(0)

  def insertPrime(v: Int): Unit = {

    primesGenerated.incrementAndGet()
    var value = v
    var index = 0

    while ( {
      if (index == numPrimes) return false
      val p = primes.get(index)
      p < value && p != 0
    }) index += 1

    while (index < numPrimes && value != 0) {
      value = primes.getAndSet(index, value)
      index += 1
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
      current.set(me, candidate.getAndIncrement())
      val myCandidate = current.get(me)

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
      for (i <- 0 until numPrimes)
        System.out.println((i + 1) + ": " + primes.get(i))
    }
  }

  def main(args: Array[String]): Unit = {
    ThreadUtil.runIndexedSystem(numWorkers, worker)
  }
}
