
sealed abstract class Chip {
  override def toString: String
}

case object NoChip extends Chip {
  override def toString: String = "."
}
case object BlackChip extends Chip {
  override def toString: String = "B"
}
case object WhiteChip extends Chip {
  override def toString: String = "W"
}

case class Node(id: Int)
case class Edge(from: Node, to: Node, color: Int)

case class GameGraph(val nodes: Set[Node], val edges: Set[Edge], val states: Vector[Chip]) {
  val node2id: Map[Node,Int] = nodes.map(n => n -> n.id).toMap
  val id2node: Map[Int,Node] = nodes.map(n => n.id -> n).toMap

  val node2neigh: Map[Node, Set[Node]] = {
    edges.foldLeft(Map.empty[Node, Set[Node]])( (acc,ele) => {
      val sofar_from = acc.get(ele.from).getOrElse(Set.empty[Node])
      val sofar_to = acc.get(ele.to).getOrElse(Set.empty[Node])
      acc.updated(ele.from,sofar_from + ele.to).updated(ele.to, sofar_to + ele.from)
    })
  }

 def update(): GameGraph = {
   this.copy(states = ???)
 }

  val node2mills: Map[Node,Map[Int,Set[Node]]] = {
    val t = edges.foldLeft(Map.empty[Node,Map[Int,Set[Node]]])( (acc,ele) => {
      def up(acc: Map[Node,Map[Int,Set[Node]]], x: Node, c: Int): Map[Node,Map[Int,Set[Node]]] = {
        val color = c
        val from = x
        val sofar = acc.getOrElse(from, Map.empty[Int, Set[Node]])
        val sofarc = sofar.getOrElse(color, Set.empty[Node])
        val updatefromcolor = sofar + (color -> (sofarc + from))
        acc + (x -> updatefromcolor)
      }
      val up1 = up(acc,ele.from,ele.color)
      up(up1,ele.to,ele.color)
    })
    t
  }

  //val t = node2mills.flatMap( (p => p._2.map( o => o._2)))


  val mills: Set[Set[Node]] = node2mills.flatMap( (p => p._2.map( o => o._2))).foldLeft(Set.empty[Set[Node]])( (acc,ele) => acc + ele)

  def checkMill(m: Set[Node]): Boolean = {
    val chips = m.map(x => states(x.id))
    if (chips.size > 1) false
    else if (chips.contains(NoChip)) false else true
  }

  def checkWhiteMill(m: Set[Node]): Boolean = {
    val chips = m.map(x => states(x.id))
    if (chips.size > 1) false
    else if (chips.contains(WhiteChip)) true else false
  }

  def checkBlackMill(m: Set[Node]): Boolean = {
    val chips = m.map(x => states(x.id))
    if (chips.size > 1) false
    else if (chips.contains(BlackChip)) true else false
  }

  def getNrMills(): Int = mills.foldLeft(0)( (acc,ele) => if (checkMill(ele)) acc + 1 else acc)
  def getNrWhiteMills(): Int = mills.foldLeft(0)( (acc,ele) => if (checkWhiteMill(ele)) acc + 1 else acc)
  def getNrBlackMills(): Int = mills.foldLeft(0)( (acc,ele) => if (checkBlackMill(ele)) acc + 1 else acc)


  def isInMill(n: Node): Boolean = node2mills.get(n).map(m => m.map(s => checkMill(s._2)).reduce((a,b) => a || b)).getOrElse(false)
  //val t = node2mills.get(n)





}
// 0 1 2 3 4 5 6 7 8 9 101112
// 0 - - - - - 1 - - - - - 2
// |           |           |
// |   3 - - - 4 - - - 5   |
// |   |       |       |   |
// |   |   6 - 7 - 8   |   |
// |   |   |       |   |   |
// 9 - 10-11       12- 13-14
// |   |   |       |   |   |
// |   |   15-16 -17   |   |
// |   |       |       |   |
// |   18- - -19 - - -20   |
// |           |           |
// 21- - - - -22 - - - - -23

object GameGraph {
  val createNMM2d: (Set[Node], Set[Edge]) = {
    val n = (0 until (8 * 3)).map(i => Node(i))
    val edges = Vector(
      Edge(n(0), n(9), 1),
      Edge(n(9), n(21), 1),

      Edge(n(3), n(10), 1),
      Edge(n(10), n(18), 1),

      Edge(n(6), n(11), 1),
      Edge(n(11), n(15), 1),

      Edge(n(1), n(4), 1),
      Edge(n(4), n(7), 1),

      Edge(n(16), n(19), 1),
      Edge(n(19), n(22), 1),

      Edge(n(8), n(12), 1),
      Edge(n(12), n(17), 1),

      Edge(n(5), n(13), 1),
      Edge(n(13), n(20), 1),

      Edge(n(3), n(14), 1),
      Edge(n(14), n(23), 1),

      Edge(n(0), n(1), 0),
      Edge(n(1), n(2), 0),

      Edge(n(3), n(4), 0),
      Edge(n(4), n(5), 0),

      Edge(n(6), n(7), 0),
      Edge(n(7), n(8), 0),

      Edge(n(9), n(10), 0),
      Edge(n(10), n(11), 0),

      Edge(n(12), n(13), 0),
      Edge(n(13), n(14), 0),

      Edge(n(15), n(16), 0),
      Edge(n(16), n(17), 0),

      Edge(n(18), n(19), 0),
      Edge(n(19), n(20), 0),

      Edge(n(21), n(22), 0),
      Edge(n(22), n(23), 0)
    )
    (n.toSet, edges.toSet)
  }
}
object NMM2d_empty extends GameGraph(nodes = GameGraph.createNMM2d._1, edges = GameGraph.createNMM2d._2 , Vector.fill(GameGraph.createNMM2d._1.size)(NoChip))