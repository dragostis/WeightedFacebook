import java.io.{PrintWriter, File}
import scala.collection.mutable.ArrayBuffer

class GuessGdf(nodes: List[Node], edges: List[Edge]) {
  def save(file: File) = {
    val output = new PrintWriter(file)

    output.println("nodedef> name,id VARCHAR(15),strength FLOAT")
    nodes.foreach(node => output.println(node.name + "," + node.id + f",${node.strength}%.3f"))

    output.println("edgedef> node1,node2")
    edges.foreach(edge => output.println(edge.node1 + "," + edge.node2))

    output.close()
  }
}

class Node(val name: String, val id: String, val strength: Float)
class Edge(val node1: String, val node2: String)