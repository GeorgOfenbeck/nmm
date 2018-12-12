import org.scalacheck.Properties
import org.scalacheck.Prop.forAll


object BoardTest extends Properties("Board") {

  val b = new Board()

  property("no match with less then 3") = forAll{

  }
}
