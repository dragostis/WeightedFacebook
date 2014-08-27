import com.restfb.types.User
import com.restfb.{DefaultFacebookClient, FacebookClient}
import java.io.File
import java.util.concurrent.Executors
import java.util.zip.ZipFile
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent._
import scala.util.{Failure, Success}
import scala.xml.XML

class GraphApi(token: String, archive: File, update: (Int) => Unit) {
  private val numberOfThreads = 500
  protected val facebookClient: FacebookClient = new DefaultFacebookClient(token)

  update(1)

  protected val guessGdf = {
    val friends = facebookClient.fetchConnection("me/friends", classOf[User]).getData

    update(3)

    val gdf = new GuessGdf
    val messages = new mutable.HashMap[String, Int]()

    val zip = new ZipFile(archive)
    zip.entries.filter(_.getName.endsWith("messages.htm")).foreach { file =>
      val html = XML.load(zip.getInputStream(file))

      html \\ "span" filter { _ \ "@class" exists (_.text == "user") } foreach { node =>
        val user = node.text

        messages.get(user) match {
          case Some(v) => messages.put(user, v + 1)
          case None => messages.put(user, 1)
        }
      }
    }

    update(10)

    val total = messages.filter((e) => friends.exists(_.getName == e._1)).foldLeft(0)(_ + _._2).toFloat

    for (friend <- friends) {
      gdf.nodes += new Node(friend.getName, friend.getId, messages.getOrElse(friend.getName, 0) / total)
    }

    implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(numberOfThreads))

    @volatile var tick = 0
    var ticks = 0

    for (i <- 0 until gdf.nodes.size) {
      for (j <- (i + 1) until gdf.nodes.size) {
        future {
          val edge = getEdge(gdf.nodes(i), gdf.nodes(j))

          this.synchronized { tick += 1 }

          edge
        } onComplete {
          case Success(edge) => this.synchronized { gdf.edges ++= edge }
          case Failure(ex) => println(ex.getMessage)
        }

        ticks += 1
      }
    }

    while (tick != ticks) {
      update((tick / ticks.toFloat * 90).ceil.toInt + 10)

      Thread.sleep(300)
    }

    gdf
  }

  def save(file: File): Unit = guessGdf.save(file)

  private def getEdge(node1: Node, node2: Node) = {
    facebookClient.fetchConnection(node1.id + "/friends/" + node2.id, classOf[User]).getData.isEmpty match {
      case false => Some(new Edge(node1, node2))
      case true => None
    }
  }
}