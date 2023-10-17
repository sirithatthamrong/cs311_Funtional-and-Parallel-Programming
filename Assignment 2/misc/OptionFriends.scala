object OptionFriends extends App {

  def lookup(xs: List[(String, String)], key: String): Option[String] = xs match {
    case Nil => None
    case h::t if h._1.equals(key) => Some(h._2)
    case _ => lookup(xs.tail, key)
  }
//  println(lookup(List(("a","xy"), ("c","pq"), ("a","je")), "a").contains("xy"))
//  println(lookup(List(("a","xy"), ("c","pq"), ("a","je")), "b").isEmpty)


  def resolve(userIdFromLoginName: String => Option[String],      // take login name, returns user id
              majorFromUserId: String => Option[String],          // take user id, returns major
              divisionFromMajor: String => Option[String],        // take major, returns division
              averageScoreFromDivision: String => Option[Double], // take division, returns avg
              loginName: String): Double = {
    userIdFromLoginName(loginName)        // extract user id from login name
      .flatMap(majorFromUserId)           // extract major from user id
      .flatMap(divisionFromMajor)         // extract division from major
      .flatMap(averageScoreFromDivision)  // extract avg score from division
      .getOrElse(0.0)
  }
//  val userIdMap = Map("Ling" -> "6480952", "Eng" -> "5880990")
//  val majorMap = Map("6480952" -> "Computer Science", "5880990" -> "Bio Technology")
//  val divisionMap = Map("Computer Science" -> "Science1", "Bio Technology" -> "Science2")
//  val scoreMap = Map("Science1" -> 85.0, "Science2" -> 95.0)
//  val loginNames = List("Ling", "Eng", "Chung")
//  loginNames.foreach { loginName =>
//    val result = resolve(
//      userIdFromLoginName => userIdMap.get(loginName),
//      majorFromUserId => majorMap.get(majorFromUserId),
//      divisionFromMajor => divisionMap.get(divisionFromMajor),
//      averageScoreFromDivision => scoreMap.get(averageScoreFromDivision),
//      loginName
//    )
//    println(s"Average score for $loginName: $result")
//  }
}
