



sealed abstract class Chip

final class NoChip extends Chip
final class BlackChip extends Chip
final class WhiteChip extends Chip

//case class Node(state: Chip = new NoChip)



case class NineMen(
                  topLeft: Chip = new NoChip,
                  topMiddle: Chip = new NoChip,
                  topRight: Chip = new NoChip,
                  middleLeft: Chip = new NoChip,
                  middleRight: Chip = new NoChip,
                  bottomLeft: Chip = new NoChip,
                  bottomRight: Chip = new NoChip,
                  bottomMiddle: Chip = new NoChip
                  )




object Board{
  val one_rec: Int = 8
  val nr_recs:Int  = 3
  val spots: Int = one_rec * nr_recs

  def empty(): Board = {
    val v = for (i <- 0 until Board.spots) yield new NoChip
    Board()
  }
}

case class Board(v: Vector[Chip]) {
  require(v.length == Board.spots)
  def ringfromVector(v: Vector[Chip]): NineMen = v match {
    case Vector(a,b,c, d,e, f,g,h) => NineMen(a,b,c,d,e,f,g,h)
    case _ => {
      assert(false, "this should not be possible with the enforced length")
      NineMen()
    }
  }


  def toVector(): Vector[Chip] = {
    ringToVector(outerRing) ++ ringToVector(middleRing) ++ ringToVector(innerRing)
  }
  
  def ringToVector(ring: NineMen): Vector[Chip] = {
    Vector(
      ring.topLeft, ring.topMiddle, ring.topRight,
      ring.middleLeft, ring.middleRight,
      ring.bottomLeft, ring.bottomMiddle, ring.bottomRight
    )
  }
  




  val outerRing: NineMen = ringfromVector(v.slice(0,Board.one_rec))
  val middleRing: NineMen = ringfromVector(v.slice(Board.one_rec,Board.one_rec*2))
  val innerRing: NineMen = ringfromVector(v.slice(Board.one_rec*2,Board.one_rec*3))

  def checkclose(a: Chip, b: Chip, c: Chip): Boolean = (a,b,c) match{
    case (x: WhiteChip, y: WhiteChip, z: WhiteChip) => true
    case (x: BlackChip, y: BlackChip, z: BlackChip) => true
    case _ => true
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
