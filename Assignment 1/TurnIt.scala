object TurnIt extends App {
  def transpose(A: List[List[Int]]): List[List[Int]] = {
    def addEach(list: List[Int], ans: List[List[Int]], count: Int, ret: List[List[Int]]): List[List[Int]] = ans match {
      case Nil =>
        if (count==0) list match {
          case Nil => ret
          case h::t => addEach(t, ans, count, ret:+List(h))
        }
        else ret
      case h::t =>
        val updated = h :+ list.head
        addEach(list.tail, t, count, ret:+updated)
    }
    def combine(list: List[List[Int]], ans: List[List[Int]], count: Int): List[List[Int]] = list match {
      case Nil => ans
      case h::t =>
        val adding = addEach(h, ans, count, Nil)
        combine(t, adding, count+1)
    }
    combine(A, Nil, 0)
  }
//    // [1,4,7], [2,5,8], [3,6,9]
//    println(transpose(List(
//      List(1, 2, 3),
//      List(4, 5, 6),
//      List(7, 8, 9))))
//    // [1,4], [2,5], [3,6]
//    println(transpose(List(
//      List(1, 2, 3),
//      List(4, 5, 6))))
//    // [1,3,5,7], [2,4,6,8]
//    println(transpose(List(
//      List(1, 2),
//      List(3, 4),
//      List(5, 6),
//      List(7, 8)
//    )))
}
