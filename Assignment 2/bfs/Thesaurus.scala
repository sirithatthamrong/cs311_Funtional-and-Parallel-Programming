import scala.io.Source
import java.io.File

object Thesaurus extends App {
  
  val defaultEncoding = "ISO8859-1"
  def load(filename: String) = {
    val source = Source.fromFile(filename)
    try {
      val lines = source.getLines()
      lines.next() // skip the 1st line, "ISO8859-1"
      def setOfWords(line: Iterator[String], count: Int): Set[String] = count match {
        case 0 => Set.empty
        case _ =>
          val nextLine = line.next()
          nextLine.substring(nextLine.lastIndexOf(")|") + 2).split('|').toSet ++ setOfWords(line, count - 1)
      }
      def myMap(file: Iterator[String], map: Map[String, Set[String]]): Map[String, Set[String]] = {
        if (file.isEmpty) map
        else
          val line = file.next()
          val key = line.substring(0, line.lastIndexOf('|'))
          val count = line.substring(line.lastIndexOf('|') + 1).toInt
          val mapUpdated = Map(key -> setOfWords(file, count))
          myMap(file, map ++ mapUpdated)
      }
      myMap(lines, Map.empty)
    } finally {
      source.close()
    }
  }
//    println(load("C:/MUIC/funpar/Assignment/a2/thesaurus_db.txt"))

  def linkage(thesaurusFile: String): String => String => Option[List[String]] = {

    def findPath(src: String, dest: String, bfsResult: Map[String, String]): Option[List[String]] = {
      def constructPath(dest: String, allPath: List[String]): List[String] = {
        if (dest.equals(src)) dest :: allPath
        else constructPath(bfsResult(dest), dest :: allPath)
      }
      if (bfsResult.contains(dest)) Some(constructPath(dest, Nil))
      else None
    }
    val data = load(thesaurusFile)
    (wordA: String) => (wordB: String) => {
        val (pathResult, distance) = GraphBFS.bfs(word => data.getOrElse(word, Set.empty), wordA)
        findPath(wordA, wordB, pathResult)
      }
  }
//  val thesaurusFile = "C:/MUIC/funpar/Assignment/a2/thesaurus_db.txt"
//  val findLinks = linkage(thesaurusFile)
//  val test_1 = findLinks("clear")("vague")
//  println(test_1) // Some(List(clear, light, faint, vague)) == 4
//  val test_2 = findLinks("logical")("illogical")
//  println(test_2) // Some(List(logical, sensible, thoughtful, bemused, confused, illogical)) == 6
}
