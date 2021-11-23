import ox.cads.locks.Lock

import java.util.concurrent.atomic.AtomicInteger
import scala.reflect.ClassTag

class LinkedArrayListLock[T: ClassTag] extends ox.cads.collection.Queue[T] {

  private val size = 10
  private val enqLock = Lock() // used to protect enqueue
  private val deqLock = Lock() // used to protect dequeue

  // Nodes used in the linked list
  private class Node {
    @volatile var next: Node = _
    val data = new Array[T](size)
    @volatile var enqIndex = 0
    var deqIndex = 0
  }

  // Head and tail of the list
  private var head = new Node()
  private var tail = head // last Node in list

  /** Enqueue x */
  def enqueue(x: T): Unit = enqLock.mutex {
    //System.out.println("b enq: " + tail)
    // If array is full create a new node
    if (tail.enqIndex == size) {
      tail.next = new Node
      tail = tail.next
    }

    //System.out.println("a enq: " + tail)
    tail.data(tail.enqIndex) = x
    tail.enqIndex += 1
  }

  /** Optionally dequeue.  Return None if the queue is empty. */
  def dequeue: Option[T] = deqLock.mutex {
    //System.out.println("b deq: " + head)
    if (head.deqIndex == size) {
      if (head.next == null)
        return None
      head = head.next // If full then advance
    }

    if (head.deqIndex == head.enqIndex)
      return None

    //System.out.println("a deq: " + head)
    head.deqIndex += 1
    Some(head.data(head.deqIndex - 1))
  }
}
