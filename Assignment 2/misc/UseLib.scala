object UseLib extends App {
  def onlyBeginsWithLower(xs: Vector[String]): Vector[String] = {
    xs.filter(n => n.nonEmpty && n(0).isLower)
  }
//  println(onlyBeginsWithLower(Vector("", "mE", "linG", "Hello")) == Vector("mE", "linG"))

  def longestString(xs: Vector[String]): Option[String] = {
    if (xs.isEmpty) None
    else Some(xs.maxBy(_.length))
  }
//  println(longestString(Vector("lingggggg", "ling")).contains("lingggggg"))

  def longestLowercase(xs: Vector[String]): Option[String] = {
    longestString(onlyBeginsWithLower(xs))
  }
//  println(longestLowercase(Vector("hAppyyyyyyyy", "SAD", "bored")).contains("hAppyyyyyyyy"))
//  print(longestLowercase(Vector("MAD")).isEmpty)
}
