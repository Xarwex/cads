import ox.cads.locks.TASLock

import scala.collection.mutable
import scala.reflect.ClassTag

class LinkedArrayList[T: ClassTag] extends ox.cads.collection.Queue[T] {

  val queue = new mutable.Queue[T]
  val enqueueLock, dequeueLock = new TASLock


  override def enqueue(value: T): Unit = synchronized {
    queue.addOne(value)
  }

  override def dequeue: Option[T] = synchronized {
    if (queue.isEmpty)
      return None
    Some(queue.dequeue)
  }
}
