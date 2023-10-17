object Happy extends App {

  def sumOfDigitsSquared(n: Int): Int = {
    def sum(n: Int, myList: List[Char]): Int = myList match {
      case Nil => n
      case h::t =>
        val convert = h.asDigit
        sum((convert*convert)+n, myList.tail)
    }
    sum(0, n.toString.toList)
  }
//  println(sumOfDigitsSquared(7) == 49)
//  println(sumOfDigitsSquared(145) == 42)
//  println(sumOfDigitsSquared(199) == 163)

    def isHappy(n: Int): Boolean = n match {
      case 1 => true
      case 4 => false
      case _ => isHappy(sumOfDigitsSquared(n))
    }
//    println(isHappy(100)) // true
//    println(isHappy(111)) // false
//    println(isHappy(1234)) // false
//    println(isHappy(989)) // true

  def kThHappy(k: Int): Int = {
    def find(n: Int, ans: Int): Int = n match {
      case 0 => ans-1
      case _ =>
        if (isHappy(ans)) find(n-1, ans+1)
        else find(n, ans+1)
    }
    find(k, 1)
  }
//  println(kThHappy(1) == 1)
//  println(kThHappy(3) == 10)
//  println(kThHappy(11) == 49)
//  println(kThHappy(19) == 97)
//  println(kThHappy(30) == 190)
}

