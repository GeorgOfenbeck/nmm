package com.ofenbeck
package nmm
package logic


case class NineMen(
                  topLeft: Chip = NoChip,
                  topMiddle: Chip = NoChip,
                  topRight: Chip = NoChip,
                  middleLeft: Chip = NoChip,
                  middleRight: Chip = NoChip,
                  bottomLeft: Chip = NoChip,
                  bottomMiddle: Chip = NoChip,
                  bottomRight: Chip = NoChip
                  )


object Board {

    sealed abstract class Position(val value: Int)

    case object  outerTopLeft extends Position(0)
    case object  outerTopMiddle extends Position(1)
    case object  outerTopRight extends Position( 2 )
    case object  outerMiddleLeft extends Position( 3)
    case object  outerMiddleRight extends Position( 4)
    case object  outerBottomLeft extends Position( 5)
    case object  outerBottomMiddle extends Position( 6)
    case object  outerBottomRight extends Position( 7)

    case object  middleTopLeft extends Position( 8)
    case object  middleTopMiddle extends Position( 9)
    case object  middleTopRight extends Position( 10)
    case object  middleMiddleLeft extends Position( 11)
    case object  middleMiddleRight extends Position( 12)
    case object  middleBottomLeft extends Position( 13)
    case object  middleBottomMiddle extends Position( 14)
    case object  middleBottomRight extends Position( 15)

    case object  innerTopLeft extends Position( 16)
    case object  innerTopMiddle extends Position( 17)
    case object  innerTopRight extends Position( 18)
    case object  innerMiddleLeft extends Position( 19)
    case object  innerMiddleRight extends Position( 20)
    case object  innerBottomLeft extends Position( 21)
    case object  innerBottomMiddle extends Position( 22)
    case object  innerBottomRight extends Position( 23)



  
  val allPositions = Vector( outerTopLeft ,
    outerTopMiddle ,
    outerTopRight ,
    outerMiddleLeft ,
    outerMiddleRight ,
    outerBottomLeft ,
    outerBottomMiddle ,
    outerBottomRight ,

    middleTopLeft ,
    middleTopMiddle ,
    middleTopRight ,
    middleMiddleLeft ,
    middleMiddleRight ,
    middleBottomLeft ,
    middleBottomMiddle ,
    middleBottomRight ,

    innerTopLeft ,
    innerTopMiddle ,
    innerTopRight ,
    innerMiddleLeft ,
    innerMiddleRight ,
    innerBottomLeft ,
    innerBottomMiddle ,
    innerBottomRight )

  val one_rec: Int = 8
  val nr_recs: Int = 3
  val spots: Int = one_rec * nr_recs


  def empty(): Board =  Board(Vector.fill(Board.spots)(NoChip))

  def ringToVector(ring: NineMen): Vector[Chip] = {
    Vector(
      ring.topLeft, ring.topMiddle, ring.topRight,
      ring.middleLeft, ring.middleRight,
      ring.bottomLeft, ring.bottomMiddle, ring.bottomRight
    )
  }


}

case class Board(v: Vector[Chip]) {
  import Board._
  require(v.length == Board.spots)
  def ringfromVector(v: Vector[Chip]): NineMen = v match {
    case Vector(a,b,c, d,e, f,g,h) => NineMen(topLeft = a, topMiddle = b,topRight = c,middleLeft = d,middleRight = e,bottomLeft = f,bottomMiddle = g,bottomRight = h)
    case _ => {
      assert(false, "this should not be possible with the enforced length")
      NineMen()
    }
  }

  def getOptions(): Vector[Board.Position] = Board.allPositions.filter(pos => getPos(pos) == NoChip)

  def toVector(): Vector[Chip] = {
    ringToVector(outerRing) ++ ringToVector(middleRing) ++ ringToVector(innerRing)
  }


  override def toString: String = {

    // 0 1 2 3 4 5 6 7 8 9 101112
    // x - - - - - x - - - - - x
    // |           |           |
    // |   x - - - x - - - x   |
    // |   |       |       |   |
    // |   |   x - x - x   |   |
    // |   |   |       |   |   |
    // x - x - x       x - x - x
    // |   |   |       |   |   |
    // |   |   x - x - x   |   |
    // |   |       |       |   |
    // |   x - - - x - - - x   |
    // |           |           |
    // x - - - - - x - - - - - x

    s"""#
        #${getPos(Board.outerTopLeft)} - - - - - ${getPos(Board.outerTopMiddle)} - - - - - ${getPos(Board.outerTopRight)}
        #|           |           |
        #|   ${getPos(Board.middleTopLeft)} - - - ${getPos(Board.middleTopMiddle)} - - - ${getPos(Board.middleTopRight)}   |
        #|   |       |       |   |
        #|   |   ${getPos(Board.innerTopLeft)} - ${getPos(Board.innerTopMiddle)} - ${getPos(Board.innerTopRight)}   |   |
        #|   |   |       |   |   |
        #${getPos(Board.outerMiddleLeft)} - ${getPos(Board.middleMiddleLeft)} - ${getPos(Board.innerMiddleLeft)}       ${getPos(Board.outerMiddleRight)} - ${getPos(Board.middleMiddleRight)} - ${getPos(Board.innerMiddleRight)}
        #|   |   |       |   |   |
        #|   |   ${getPos(Board.innerBottomLeft)} - ${getPos(Board.innerBottomMiddle)} - ${getPos(Board.innerBottomRight)}   |   |
        #|   |       |       |   |
        #|   ${getPos(Board.middleBottomLeft)} - - - ${getPos(Board.middleBottomMiddle)} - - - ${getPos(Board.middleBottomRight)}   |
        #|           |           |
        #${getPos(Board.outerBottomLeft)} - - - - - ${getPos(Board.outerBottomMiddle)} - - - - - ${getPos(Board.outerBottomRight)}
      """.stripMargin('#')
  }


  val outerRing: NineMen = ringfromVector(v.slice(0,Board.one_rec))
  val middleRing: NineMen = ringfromVector(v.slice(Board.one_rec,Board.one_rec*2))
  val innerRing: NineMen = ringfromVector(v.slice(Board.one_rec*2,Board.one_rec*3))


  def setPos(pos: Board.Position, state: Chip): Board = Board(v.updated(pos.value,state))
  def getPos(pos: Board.Position): Chip = v(pos.value)

  def checkclose(a: Chip, b: Chip, c: Chip): Boolean = (a,b,c) match {

    case (x: WhiteChip.type, y: WhiteChip.type, z: WhiteChip.type) => true
    case (x: BlackChip.type, y: BlackChip.type, z: BlackChip.type) => true
    case _ => false
  }

  def checkTop(ring: NineMen): Boolean = checkclose(ring.topLeft,ring.topMiddle, ring.topRight)
  def checkBottom(ring: NineMen): Boolean = checkclose(ring.bottomLeft, ring.bottomMiddle, ring.bottomRight)
  def checkLeft(ring: NineMen): Boolean = checkclose(ring.topLeft, ring.middleLeft, ring.bottomLeft)
  def checkRight(ring: NineMen): Boolean = checkclose(ring.topRight, ring.middleRight, ring.bottomRight)

  def checkRing(ring: NineMen): Boolean = checkTop(ring) || checkBottom(ring) || checkLeft(ring) || checkRight(ring)

  def checkCrossLeft: Boolean = checkclose(innerRing.middleLeft, middleRing.middleLeft, outerRing.middleLeft)
  def checkCrossRight: Boolean = checkclose(innerRing.middleRight, middleRing.middleRight, outerRing.middleRight)
  def checkCrossTop: Boolean = checkclose(innerRing.topMiddle, middleRing.topMiddle, outerRing.topMiddle)
  def checkCrossBottom: Boolean = checkclose(innerRing.bottomMiddle, middleRing.bottomMiddle, outerRing.bottomMiddle)

  def check(): Boolean = checkRing(innerRing) || checkRing(outerRing) || checkRing(middleRing) || checkCrossLeft || checkCrossRight || checkCrossTop || checkCrossBottom
}
