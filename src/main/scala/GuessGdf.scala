import java.io.{PrintWriter, File}
import scala.collection.mutable.ArrayBuffer

class GuessGdf() {
  val nodes = new ArrayBuffer[Node]()
  val edges = new ArrayBuffer[Edge]()

  def save(file: File): Unit = {
    val output = new PrintWriter(file)

    output.println("nodedef> name,id VARCHAR(15),strength FLOAT")
    nodes.foreach(node => output.println(node.name + "," + node.id + f",${node.strength}%.3f"))

    output.println("edgedef> node1,node2")
    edges.foreach(edge => output.println(edge.node1.name + "," + edge.node2.name))

    output.close()
  }
}

class Node(val name: String, val id: String, val strength: Float)
class Edge(val node1: Node, val node2: Node)