package operations

import org.junit.Assert._
import org.junit.Test
import data.Datum._
import data._

class CurrentAirborneStateTest {
  import scala.concurrent.duration._

  val id = AircraftAddress(12345)

  val pv = CurrentAirborneState.processValues(Duration(1, SECONDS), Duration(15, SECONDS))_

  @Test
  def empty {
    assertEquals(None, pv(Seq(), None))
  }

  @Test
  def emptyCallsign {
    assertEquals(Some(CurrentAirborneState(Timestamp(10), Some("callsign"), None, None)),
      pv(
        Seq(
          IdentificationMessage(Timestamp(10),
            id,
            null,
            "callsign")),
        None))
  }

  @Test
  def emptySurfacePosition {
    assertEquals(Some(CurrentAirborneState(Timestamp(10), None, None, None)),
      pv(
        Seq(
          SurfacePositionMessage(Timestamp(10),
            id,
            Unsynchronized,
            OddCompactPositionRepresentation(0, 0),
            SurfaceHorizontalContainmentRadius(TypeCode(0)))),
        None))
  }

  @Test
  def emptyAirbornePosition {
    assertEquals(Some(CurrentAirborneState(Timestamp(10), None, None, Some(OddCompactPositionRepresentation(0, 0)))),
      pv(
        Seq(
          AirbornePositionMessage(Timestamp(10),
            id,
            No_Condition_Information,
            BarometricAltitude(10),
            Unsynchronized,
            OddCompactPositionRepresentation(0, 0),
            UnknownHorizontalContainmentRadius)),
        None))
  }

  @Test
  def oodEvenAirbornePosition {
    assertEquals(Some(CurrentAirborneState(Timestamp(11), None, Some(Position(0, 0)), Some(EvenCompactPositionRepresentation(0, 0)))),
      pv(
        Seq(
          AirbornePositionMessage(Timestamp(11),
            id,
            No_Condition_Information,
            BarometricAltitude(10),
            Unsynchronized,
            EvenCompactPositionRepresentation(0, 0),
            UnknownHorizontalContainmentRadius)),
        Some(CurrentAirborneState(Timestamp(10), None, None, Some(OddCompactPositionRepresentation(0, 0))))))
  }
}