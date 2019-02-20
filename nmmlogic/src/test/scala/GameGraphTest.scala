
import org.scalacheck.{Gen, Properties, Shrink}
import org.scalacheck.Prop.forAll


object GameGraphTest extends Properties("GraphTest") {

  implicit val shrinkExpr: Shrink[GameGraph] = Shrink({
    case ggz: GameGraph with GameGraph2Viz => {
      println(ggz.toString())
      Stream.empty
    }
    case gg: GameGraph => {
      val gprint = new GameGraph(gg.nodes,gg.edges,gg.states) with GameGraph2Viz {
        val positions: Map[Node,Pos] = {
          val tups = nodes.map ( n => n.id match{
            case 0 => (Pos(0,0,0),n)
            case 1 => (Pos(6,0,0),n)
            case 2 => (Pos(12,0,0),n)
            case 3 => (Pos(2,2,0),n)
            case 4 => (Pos(6,2,0),n)
            case 5 => (Pos(10,2,0),n)
            case 6 => (Pos(4,4,0),n)
            case 7 => (Pos(6,4,0),n)
            case 8 => (Pos(8,4,0),n)
            case 9 => (Pos(0,6,0),n)
            case 10 => (Pos(2,6,0),n)
            case 11 => (Pos(4,6,0),n)
            case 12 => (Pos(8,6,0),n)
            case 13 => (Pos(10,6,0),n)
            case 14 => (Pos(12,6,0),n)
            case 15 => (Pos(4,8,0),n)
            case 16 => (Pos(6,8,0),n)
            case 17 => (Pos(8,8,0),n)
            case 18 => (Pos(2,10,0),n)
            case 19 => (Pos(6,10,0),n)
            case 20 => (Pos(10,10,0),n)
            case 21 => (Pos(0,12,0),n)
            case 22 => (Pos(6,12,0),n)
            case 23 => (Pos(12,12,0),n)
          })
          tups.map(x => x.swap).toMap
        }
      }
      println(gprint)
      Stream.empty
    }
    case _ => Stream.empty
  })

  def genEmptyGGraph(): Gen[GameGraph] = NMM2d_empty

  def genPlaceOne(board: GameGraph, color: Chip): Gen[GameGraph] = {
    for {
      pick <- Gen.oneOf(board.nodes.toVector)
    } yield board.update(pick, color)
  }

  def genPlaceN(board: GameGraph, n: Int, color: Chip): Gen[GameGraph] = {
    if (n <= 0) board
    else {
      for {
        onePlacement <- genPlaceOne(board, color)
        next <- genPlaceN(onePlacement, n - 1, color)
      } yield next
    }
  }


  def genRandPlacedBoard(w: Int, b: Int): Gen[GameGraph] = {
    for {
      whiteplaced <- genPlaceN(NMM2d_empty, w, WhiteChip)
      bothplaced <- genPlaceN(whiteplaced, b, BlackChip)
    } yield bothplaced
  }

  def genRandBoard(): Gen[GameGraph] = {
    val b = Board.empty()
    for {
      totalchip <- Gen.chooseNum[Int](0, Board.spots)
      white <- Gen.chooseNum(0, totalchip)
      black <- Gen.chooseNum(0, totalchip - white)
      board <- (genRandPlacedBoard(white, black))
    } yield board
  }

  property("empty board has no mill") = forAll(genEmptyGGraph())(board => board.getNrMills() == 0)

  val empty = NMM2d_empty

  property("set one") = forAll(genPlaceOne(NMM2d_empty,WhiteChip))(board => {
    board.states.filter(c => c == WhiteChip).size == 1
  })

  property("set one and unset") = forAll(genPlaceOne(NMM2d_empty,WhiteChip))(board => {
    val (chip,idx) = board.states.zipWithIndex.filter {case (c, idx) => c == WhiteChip}.head
    val emptyagain = board.update(board.id2node(idx),NoChip)
    NMM2d_empty.states == emptyagain.states
  })

  property("set one diff empty") = forAll(genPlaceOne(NMM2d_empty,WhiteChip))(board => {
    NMM2d_empty.states != board.states
  })

  //val twoofEach = genRandPlacedBoard(2,2)


  property("everyone has at least 2 neighbours") = forAll(genEmptyGGraph())(board => {
    board.node2neigh.map{
      case (n,s) => {
        if (s.size < 2) println(n.id)
        s.size
      }
    }.min >= 2
  })


  property("every node is part of at least 2 colors") = forAll(genEmptyGGraph())(board =>
    board.node2mills.filter{ case (n, cm) => cm.size > 1 }.size == board.node2mills.size)

  property("every node is part of exactly 2 colors") = forAll(genEmptyGGraph())(board =>
    board.node2mills.filter{ case (n, cm) => cm.size == 2 }.size == board.node2mills.size)


  property("xmills are sets of 3 in mills") =  forAll(genEmptyGGraph())(board => {
    val x = board.mills.filter(p => p.size != 3).isEmpty
    if (x == false)
      println(".")
    x
  })


  property("set one alone is not mill") = forAll(genPlaceOne(NMM2d_empty,WhiteChip))(board => {
    val (chip,idx) = board.states.zipWithIndex.filter {case (c, idx) => c == WhiteChip}.head
    val node = board.id2node(idx)
    !board.checkMill(board.node2mills(node).head._2)
  })


  property("1 cannot check") = forAll(genRandPlacedBoard(1,0))(board => board.getNrMills() == 0)
  property("oneWhite") = forAll(genRandPlacedBoard(1,0))(board => board.getNrMills() == 0)


  /*
  property("every node is bidrectional in terms of mills") = forAll(genEmptyGGraph())(board => {
    val m = board.node2mills

    val one2n = m.map{
      case (node, cmap) => {
        node -> cmap.flatMap { case (color, nodes) => nodes }.toSet
      }
    }

    val res = one2n.map{
      case (node, nset) => {
        val t = nset.map(other => one2n(other).contains(node))
        t.contains(false) //true if we have one miss
      }
    }.toSet
    !res.contains(true)
  })*/

  /*
  property("mills are sets of 3 in node2mills") =  forAll(genEmptyGGraph())(board => {
    val m = board.node2mills

    val one2n = m.flatMap{
      case (node, cmap) => { cmap.map { case (color, nodes) => {

        if (nodes.size == 3) {
          //println(nodes)
          true
        }
        else {
          //println(nodes)
          false
        }
      }
      }.toSet } }.toSet
    one2n.contains(false)
  })*/

  /*
  def checkMill(m: Set[Node]): Boolean = {
    val chips = m.map(x => states(x.id))
    if (chips.size > 1) false
    else if (chips.contains(NoChip)) false else true
  }
  */

  //val twoofEach = genRandPlacedBoard(2,2)
  //property("2 of each cannot check") = forAll(twoofEach)(board => board.getNrMills() == 0)

}
