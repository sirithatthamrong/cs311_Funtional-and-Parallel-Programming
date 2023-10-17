object Zombies extends App {
  def countBad(hs: List[Int]): Int = {
    def count(list: List[Int], num: Int): Int = list match {
      case Nil => 0
      case h::Nil => if (h > num) 1 else 0
      case _ => count(list.slice(0, list.length/2), num) + count(list.slice(list.length/2, list.length), num)
    }
    def check(list: List[Int]): Int = list match {
      case Nil => 0
      case h::t => count(t, h) + check(t)
    }
    check(hs)
  }
//    println(countBad(List(35,22,10)) == 0)
//    println(countBad(List(3,1,4,2)) == 3)
//    println(countBad(List(5,4,11,7)) == 4)
//    println(countBad(List(1,7,22,13,25,4,10,34,16,28,19,31)) == 49)
//    println(countBad(List(1,2,3,4)) == 6)
}
