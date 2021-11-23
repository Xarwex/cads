import ox.cads.locks.Lock

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.reflect.ClassTag

class LinkedArrayListLockFree[T: ClassTag] extends ox.cads.collection.Queue[T] {

  private val size = 10

  private class Node {
    val next = new AtomicReference[Node](null)
    val data = new Array[AtomicReference[T]](size)

    // Use pointers instead of indices
    val enqIndex = new AtomicInteger(0)
    val deqIndex = new AtomicInteger(0)

    data.foreach(elem => elem.set(null.asInstanceOf[T]))
  }

  private val firstNode = new Node
  private val head = new AtomicReference[Node](firstNode)
  private val tail = new AtomicReference[Node](firstNode)

  override def enqueue(value: T): Unit = {
    var done = false
    while (!done) {
      val myTail = tail.get()
      val next = myTail.next.get()

      // Proper tail
      if (myTail == tail.get())
        if (next == null) {

          val enqueueIndex = myTail.enqIndex.get()
          if (enqueueIndex < size) {
            if (myTail.enqIndex.compareAndSet(enqueueIndex, enqueueIndex + 1)) { // Partial enqueue for the new element - reserving the index
              myTail.data(enqueueIndex).set(value) // actual enqueue
              done = true
            }
          } else {
            val newNode = new Node
            if (myTail.next.compareAndSet(null, newNode))
              tail.compareAndSet(myTail, newNode)
          }
        } else
          tail.compareAndSet(myTail, next)
    }
  }

  override def dequeue: Option[T] = {
    var done = false
    var result: Option[T] = None
    while (!done) {
      val myHead = head.get
      val myTail = tail.get
      val next = myHead.next.get
      val deqInd = myHead.deqIndex.get()
      val enqInd = myHead.enqIndex.get()

      if (myHead == head.get) {
        if (deqInd == enqInd) {
          if (myHead == myTail) {
            if (next == null) { // empty queue, return null
              result = None
              done = true
            }
            else // new item partially enqueued
              tail.compareAndSet(myTail, next) // try to advance tail
          } else
            head.compareAndSet(myHead, next)
        }
        else { // non-empty queue
          val res = myHead.data(deqInd).get()
          if (res != null && myHead.deqIndex.compareAndSet(deqInd, deqInd + 1)) {
            result = Some(res)
            done = true
          }
        }
      }
    }
    result
  }
}
