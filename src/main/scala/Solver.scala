import java.io.File
import java.util.zip.ZipFile
import scala.collection.JavaConversions._
import scala.io.Source
import scala.xml.XML

class Solver(graph: File, archive: File) {
  private val guessGdf = {
    val zip = new ZipFile(archive)

    val messages = zip.entries.filter(_.getName.endsWith("messages.htm")).map { file =>
      val html = XML.load(zip.getInputStream(file))

      val users = html \\ "span" filter { _ \ "@class" exists (_.text == "user") } map(_.text)

      users.groupBy(u => u).mapValues(_.size)
    }.next()
    val total = messages.map(_._2).sum.toFloat

    val (nodeLines, edgeLines) = Source.fromFile(graph)("UTF-8").getLines().span(line => !line.startsWith("edgedef"))

    val nodes = nodeLines.toList.tail.map { line =>
      val data = line.split(",")

      val messageCount = messages.get(data(1)) match {
        case Some(count) => count
        case None => 0
      }

      new Node(data(1), data(0), messageCount / total)
    }

    val edges = edgeLines.toList.tail.map { line =>
      val data = line.split(",")

      new Edge(data(0), data(1))
    }

    new GuessGdf(nodes, edges)
  }

  def save(file: File) = guessGdf.save(file)
}
