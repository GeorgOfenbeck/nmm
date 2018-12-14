import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll


object BoardTest extends Properties("Board") {


  def genEmptyBoard(): Gen[Board] = {
    Board.empty()
  }


  def genPlaceOne(board: Board, color: Chip): Gen[Board] = {
    for {
      pick <- Gen.oneOf(board.getOptions())
    } yield board.setPos(pick,color)
  }

  def genPlaceN(board: Board, n: Int, color: Chip): Gen[Board] = {
    if (n <= 0) board
    else {
      for {
        onePlacement <- genPlaceOne(board,color)
        next <- genPlaceN(onePlacement,n-1,color)
      } yield next
    }
  }


  def genRandPlacedBoard(w: Int, b: Int): Gen[Board] = {
    for {
      whiteplaced <- genPlaceN(Board.empty(),w,WhiteChip)
      bothplaced <- genPlaceN(whiteplaced,b,BlackChip)
    } yield bothplaced
  }

  def genRandBoard(): Gen[Board] = {
    val b = Board.empty()
    for {
      totalchip <- Gen.chooseNum[Int](0,Board.spots)
      white <- Gen.chooseNum(0, totalchip)
      black <- Gen.chooseNum(0, totalchip-white)
      board <- (genRandPlacedBoard(white,black))
    } yield  board
  }

  def genMatchBoard(color: Either[WhiteChip.type,BlackChip.type]): Gen[Board] = {
    /*
    val c: Chip = color.fold[Chip](fa => fa,fb => fb)

    val matches = Vector(
      Board.empty().setPos(Board.outerTopLeft,c).setPos(Board.outerTopMiddle,c).setPos(Board.outerTopRight,c),
      Board.empty().setPos(Board.middleTopLeft,c).setPos(Board.middleTopMiddle,c).setPos(Board.middleTopRight,c),
      Board.empty().setPos(Board.innerTopLeft,c).setPos(Board.innerTopMiddle,c).setPos(Board.innerTopRight,c),

      Board.empty().setPos(Board.outerBottomLeft,c).setPos(Board.outerBottomMiddle,c).setPos(Board.outerBottomRight,c),
      Board.empty().setPos(Board.middleBottomLeft,c).setPos(Board.middleBottomMiddle,c).setPos(Board.middleBottomRight,c),
      Board.empty().setPos(Board.innerBottomLeft,c).setPos(Board.innerBottomMiddle,c).setPos(Board.innerBottomRight,c),

      Board.empty().setPos(Board.outerBottomLeft,c).setPos(Board.outerBottomMiddle,c).setPos(Board.outerBottomRight,c),
      Board.empty().setPos(Board.middleBottomLeft,c).setPos(Board.middleBottomMiddle,c).setPos(Board.middleBottomRight,c),
      Board.empty().setPos(Board.innerBottomLeft,c).setPos(Board.innerBottomMiddle,c).setPos(Board.innerBottomRight,c)
    ) */ ???
  }

  val randomBoard = genRandPlacedBoard(1,0)


  property("toVector / fromVector symetry") = forAll(randomBoard)(board => {
    val v = board.toVector()
    val board2 = Board(v)
    board == board2
  })

  val twoofEach = genRandPlacedBoard(2,2)
  property("2 of each cannot check") = forAll(twoofEach)(board => !board.check)



  //property("no match with less then 3") = forAll(g1: emptyGen)()

}
