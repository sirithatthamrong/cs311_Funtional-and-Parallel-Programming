object Roman extends App {
  def toRoman(n: Int): String = {

    val romanNumerals: List[(Int, String)] = List(
                                            (1000, "M"), (900, "CM"), (500, "D"), (400, "CD"),
                                            (100, "C"), (90, "XC"), (50, "L"), (40, "XL"),
                                            (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")
                                            )

    def toRomanRec(numReal: Int, list: List[(Int, String)], ans: String): String = list match {
        case Nil => ans
        case (value, sym)::t =>
          if (numReal>=value) toRomanRec(numReal-value, list, ans+sym)
          else toRomanRec(numReal, t, ans)
    }
    toRomanRec(n, romanNumerals, "")
  }
//  println(toRoman(3) == "III")
//  println(toRoman(6) == "VI")
//  println(toRoman(8) == "VIII")
//  println(toRoman(44) == "XLIV")
//  println(toRoman(58) == "LVIII")
//  println(toRoman(101) == "CI")
//  println(toRoman(600) == "DC")
//  println(toRoman(1561) == "MDLXI")
//  println(toRoman(1984) == "MCMLXXXIV")
//  println(toRoman(3999) == "MMMCMXCIX")
}
