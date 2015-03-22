package datum

import org.junit.Assert._
import org.junit.Test
import data.Datum._
import data._

class CPRTest {

  /**
   * Horizontal containment radius is 185.2 m
   * Altitude is 10972.800000000001 m  (36000 ft)
   * [c0ffee]: Now at position (47.00724792480469,8.025993347167969) (global)
   * Horizontal containment radius is 185.2 m
   * Altitude is 10972.800000000001 m  val two = hex"8D75804B580FF6B283EB7A157117"
   *
   */
  @Test
  def matcherJavaTest {

    val odd = OddCompactPositionRepresentation(92249, 113957)
    val even = EvenCompactPositionRepresentation(109385, 116887)

    assertEquals(Some(Position(47.00626179323358, 8.025442270132212)), AirborneCPR.global(odd, even))
    assertEquals(Some(Position(47.00724792480469, 8.025993347167969)), AirborneCPR.global(even, odd))

    assertEquals(EvenCompactPositionRepresentation(109363, 116879), AirborneCPR(47.00626179323358, 8.025442270132212, false))
    assertEquals(OddCompactPositionRepresentation(92249, 113957), AirborneCPR(47.00626179323358, 8.025442270132212, true))

    assertEquals(Position(47.00626179323358, 8.025442270132212), AirborneCPR.local(odd, Position(47.00724792480469, 8.025993347167969)))
    assertEquals(Position(47.00724792480469, 8.025993347167969), AirborneCPR.local(even, Position(47.00724792480469, 8.025993347167969)))

  }

  /**
   * http://www.lll.lu/~edward/edward/adsb/DecodingADSBposition.html
   *
   * one, two
   * Lon =  123.889128586342
   * Lat =  10.2162144547802
   *
   *
   * 2nd : Altitude is 2175
   */
  @Test
  def matcherEdwardTest {

    val one = EvenCompactPositionRepresentation(92095, 39846)
    val two = OddCompactPositionRepresentation(88385, 125818)

    assertEquals(Some(Position(10.21621445478019, 123.8891285863416)), AirborneCPR.global(two, one))
    assertEquals(Some(Position(10.215774536132812, 123.88881877317266)), AirborneCPR.global(one, two))

    assertEquals(EvenCompactPositionRepresentation(92105, 39853), AirborneCPR(10.21621445478019, 123.8891285863416, false))
    assertEquals(OddCompactPositionRepresentation(88385, 125818), AirborneCPR(10.21621445478019, 123.8891285863416, true))
  }

}