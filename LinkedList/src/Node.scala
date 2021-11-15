import scala.reflect.ClassTag

class Node[T: ClassTag] {

  private val size = 10
  private val data = new Array[T](size)

  // Used for
  @volatile
  private var index = 0

  private var next: Node[T] = null

  def addData(element: T): Node[T] = {
    if (index < 10) {
      data(index) = element
      index += 1
    } else {
      next = this.

    }
  }
}
