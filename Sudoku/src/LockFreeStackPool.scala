import ox.cads.collection.{LockFreeStack, Pool}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.concurrent.TrieMap

class LockFreeStackPool[T] extends Pool[T] {

  val stack = new LockFreeStack[T]
  val map = new TrieMap[Int, Any]()

  override def add(x: T): Unit = {
    val hash = x.hashCode()
    if (!map.contains(hash)) {
      map.addOne(hash, null)
      stack.push(x)
    } else
      println("skipped")
  }

  //override def add(x: T): Unit = stack.push(x)

  override def get: Option[T] = stack.pop
}
