object Spaghetti extends App {

  def spaghetti: Stream[String] = {
    def next(prev: Char, str: String, count: Int): String = str match {
      case "" => count.toString + prev
      case _ if prev.equals(str.head) => next(prev, str.tail, count+1)
      case _ => count.toString + prev + next(str.head, str.tail, 1)
    }
    lazy val ans: Stream[String] = "1" #:: ans.map(x => next(x.head, x.tail, 1))
    ans
  }
//  println(spaghetti.take(1).toList) // List(1)
//  println(spaghetti.take(3).toList) // List(1, 11, 21)
//  println(spaghetti.take(7).toList) // List(1, 11, 21, 1211, 111221, 312211, 13112221)

  def ham: Stream[String] = {
    def eachN(n: Int): Stream[String] = n match {
      case 1 => "0" #:: "1" #:: Stream.empty
      case _ => eachN(n-1).map(x => "0"+x) #::: eachN(n-1).reverse.map(x => "1"+x)
    }
    Stream.from(1).flatMap(x => eachN(x))
  }
//  println(ham.take(5).toList) // List(0, 1, 00, 01, 11)
//  println(ham.take(10).toList) // List(0, 1, 00, 01, 11, 10, 000, 001, 011, 010)
}
