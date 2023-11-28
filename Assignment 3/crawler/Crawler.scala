import java.net.URL
import org.jsoup.*
import org.jsoup.nodes.{Document, Element}
import collection.JavaConverters.*
import scala.util.*
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Crawler {
  sealed case class WebStats(
                              // the total number of (unique) files found
                              numFiles: Int,
                              // the total number of (unique) file extensions (.jpg is different from .jpeg)
                              numExts: Int,
                              // a map storing the total number of files for each extension.
                              extCounts: Map[String, Int],
                              // the total number of words in all html files combined, excluding
                              // all html tags, attributes and html comments.
                              totalWordCount: Long
                            )

  var visited: Set[String] = Set()
  var extCounts: Map[String, Int] = Map[String, Int]()
  var totalWordCount: Long = 0

  def extractLinks(doc: Document): Set[String] = {
    doc.select("a[href]").asScala.map(link => link.attr("abs:href")).toSet
  }

  def extractResources(doc: Document, selector: String): Set[String] = {
    doc.select(selector).asScala.map(link => link.attr("abs:src")).toSet
  }

  def adjustLinks(allLinks: Set[String]): Set[String] = {
    allLinks.flatMap(link => {
      val check: String = link.split('/').lastOption.getOrElse("")
      if (!check.contains('.')) Set.empty
      else if (check.contains('\\')) Set(link.substring(0, link.lastIndexOf("\\")))
      else if (check.contains('?')) Set(link.substring(0, link.lastIndexOf("?")))
      else if (check.contains('#')) Set(link.substring(0, link.lastIndexOf("#")))
      else if (check.last.equals('/')) Set(check + "index.html")
      else Set(link)
    })
  }

  def nbrs(url: String): Future[Set[String]] = {
    val p = Promise[Set[String]]
    try {
      visited += url
//      println(url)
      val documents: Document = Jsoup.connect(url).ignoreContentType(true).ignoreHttpErrors(true).get()
      val links = extractLinks(documents)
      val src = extractResources(documents, "[src]")
//      println(src)
      val allLinks: Set[String] = links ++ src
      val adjustedLinks: Set[String] = adjustLinks(allLinks)
//      println(adjustedLinks)

      val wordCount = Jsoup.connect(url).ignoreContentType(true).ignoreHttpErrors(true)
        .execute().parse().text()
        .toLowerCase().split("[.,!?:;/\\s]+").filter(_.matches("^[a-z]+"))
      totalWordCount += wordCount.length

      p.success(adjustedLinks)
    } catch {
      case e: Throwable => p.failure(e)
    }
    p.future
  }

  def expand(frontiers: Set[String], basePath: String): Future[Set[String]] = {
    val futureLinks: Set[Future[Set[String]]] = frontiers.map(link => {
      if (link.contains(URL(basePath).getHost) && !visited.contains(link)) nbrs(link)
      else Future.successful(Set.empty)
    })
    Future.sequence(futureLinks).map(_.flatten)
  }

  def bfs(basePath: String): Unit = {
    var frontiers: Set[String] = Await.result(nbrs(basePath), Duration.Inf)
    while (frontiers.nonEmpty) {
      val newFrontierFuture: Future[Set[String]] = expand(frontiers, basePath)
      frontiers = Await.result(newFrontierFuture, Duration.Inf)
    }
  }

  def crawlForStats(basePath: String): WebStats = {
    bfs(basePath)

    visited.foreach(url =>
      val ext = url.substring(url.lastIndexOf("."))
      if (!url.equals(basePath)) {
        if (extCounts.contains(ext)) {
          extCounts += (ext, extCounts(ext) + 1)
        } else extCounts += (ext, 1)
      }
    )
    WebStats(visited.size, extCounts.size, extCounts, totalWordCount)
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.currentTimeMillis()

    val sampleBasePath = "https://cs.muic.mahidol.ac.th/courses/ooc/api/"
    val ws = crawlForStats(sampleBasePath)
    println(ws)

    val endTime = System.currentTimeMillis()
    val elapsedMinutes = (endTime - startTime) / 60000.0 // minutes
    println(s"Elapsed time: $elapsedMinutes minutes")
  }
}
