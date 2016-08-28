package data

import scodec.bits.ByteVector
import scala.concurrent.duration.Duration

//http://adsb.tc.faa.gov/WG3_Meetings/Meeting30/1090-WP30-21-Appendix_A%20Mods.pdf

object Datum {

  sealed trait SurveillanceStatus
  case object No_Condition_Information extends SurveillanceStatus
  case object Permanent_Alert extends SurveillanceStatus
  case object Temporary_Alert extends SurveillanceStatus
  case object SPI_Condition extends SurveillanceStatus

  sealed trait TimeSynchronization
  case object Synchronized extends TimeSynchronization
  case object Unsynchronized extends TimeSynchronization

  trait HorizontalContainmentRadius {
    def meters(nicA: Option[NicA]): Option[Float];
  }

  case class DerivedHorizontalContainmentRadius(nicB: NicB, tc: TypeCode) extends HorizontalContainmentRadius {
    def meters(nicA: Option[NicA]): Option[Float] = tc.value match {
      case 9 => Some(7.5F)
      case 10 => Some(25.0F)
      case 11 if nicB == NicB(1) && nicA == Some(NicA(1)) => Some(75F)
      case 11 => Some(185.2F)
      case 12 => Some(370.4F)
      case 13 if nicB == NicB(0) && nicA == Some(NicA(0)) => Some(925F)
      case 13 if nicB == NicB(1) && nicA == Some(NicA(0)) => Some(555.6F)
      case 13 => Some(1111.2F)
      case 14 => Some(1852F)
      case 15 => Some(3704F)
      case 16 if nicB == NicB(1) && nicA == Some(NicA(1)) => Some(7408F)
      case 16 => Some(14816F)
      case 17 => Some(37040F)
      case _ => None
    }
  }

  //no consistent spec
  case class SurfaceHorizontalContainmentRadius(tc: TypeCode) extends HorizontalContainmentRadius {
    def meters(nicA: Option[NicA]): Option[Float] = tc.value match {
      case 5                          => Some(7.5F)
      case 6 if nicA == Some(NicA(1)) => Some(25F)
      case 6                          => Some(75F)
      case 7                          => Some(185.2F)
      case _                          => None
    }
  }

  case object UnknownHorizontalContainmentRadius extends HorizontalContainmentRadius {
    def meters(nicA: Option[NicA]): Option[Float] = None;
  }

  case class KnownHorizontalContainmentRadius(radius: Float) extends HorizontalContainmentRadius {
    def meters(nicA: Option[NicA]): Option[Float] = Some(radius);
  }

  object KnownHorizontalContainmentRadius {
    def apply(tc: TypeCode): HorizontalContainmentRadius =
      tc.value match {
        case 20 => KnownHorizontalContainmentRadius(7.5F)
        case 21 => KnownHorizontalContainmentRadius(25.0F)
        case 22 => UnknownHorizontalContainmentRadius
      }
    def typeCode(radius: HorizontalContainmentRadius): TypeCode = radius match {
      case KnownHorizontalContainmentRadius(value) if value <= 7.5F => TypeCode(20)
      case KnownHorizontalContainmentRadius(value) if value <= 25F => TypeCode(21)
      case UnknownHorizontalContainmentRadius => TypeCode(22)
      case _ => TypeCode(-1)
    }
  }

  sealed trait Altitude extends Any {
    def feet: Option[Int]
  }
  case class BarometricAltitude(height: Int) extends Altitude {
    def feet: Option[Int] = Some(height)
  }
  case class GNSSAltitude(height: Int) extends Altitude {
    def feet: Option[Int] = Some(height)
  }
  case object UnknownAltitude extends Altitude {
    def feet: Option[Int] = None
  }

  case class AircraftAddress(value: Int) extends AnyVal {
    override def toString = f"Aircraft($value%H)"
  }
  object AircraftAddress {
    def apply(hex: String): AircraftAddress = AircraftAddress(Integer.parseInt(hex, 16))
  }

  case class NicA(value: Int) extends AnyVal
  case class NicB(value: Int) extends AnyVal
  case class TypeCode(value: Int) extends AnyVal
  case class SubTypeCode(value: Int) extends AnyVal
  case class Timestamp(value: Long) extends AnyVal {
    def olderThan(duration: Duration): Boolean = System.currentTimeMillis - value > duration.toMillis
    def moreRecentThan(o: Timestamp): Boolean = value > o.value
    def noOlderThan(newer: Timestamp, duration: Duration) = newer.value - value < duration.toMillis
  }

  sealed trait EmitterCategory {
    def tc: Int
    def category: Int
  }

  trait EmitterCategoryA extends EmitterCategory {
    val tc = 4
  }

  case object NoEmitterCategoryA extends EmitterCategoryA {
    val category = 0
  }

  case object Light extends EmitterCategoryA {
    val category = 1
  }
  case object Small extends EmitterCategoryA {
    val category = 2
  }
  case object Large extends EmitterCategoryA {
    val category = 3
  }

  case object HighVortexLarge extends EmitterCategoryA {
    val category = 4
  }

  case object Heavy extends EmitterCategoryA {
    val category = 5
  }
  case object HighPerformance extends EmitterCategoryA {
    val category = 6
  }

  case object Rotocraft extends EmitterCategoryA {
    val category = 7
  }

  trait EmitterCategoryB extends EmitterCategory {
    val tc = 3
  }

  case object NoEmitterCategoryB extends EmitterCategoryB {
    val category = 0
  }

  case object Glider extends EmitterCategoryB {
    val category = 1
  }

  case object LighterThanAir extends EmitterCategoryB {
    val category = 2
  }

  case object Parachutist extends EmitterCategoryB {
    val category = 3
  }
  case object UltraLight extends EmitterCategoryB {
    val category = 4
  }

  case object UAV extends EmitterCategoryB {
    val category = 6
  }

  case object Space extends EmitterCategoryB {
    val category = 7
  }

  trait EmitterCategoryC extends EmitterCategory {
    val tc = 2
  }

  case object NoEmitterCategoryC extends EmitterCategoryC {
    val category = 0
  }

  case object EmergencyVehicle extends EmitterCategoryC {
    val category = 1
  }

  case object ServiceVehicle extends EmitterCategoryC {
    val category = 2
  }

  case object PointObstacle extends EmitterCategoryC {
    val category = 3
  }
  case object ClusterObstacle extends EmitterCategoryC {
    val category = 4
  }

  case object LineObstacle extends EmitterCategoryC {
    val category = 5
  }

  trait EmitterCategoryD extends EmitterCategory {
    val tc = 1
  }

  case object NoEmitterCategoryD extends EmitterCategoryD {
    val category = 0
  }

  case class ReservedEmitterCategory(tc: Int, category: Int) extends EmitterCategory

  object EmitterCategory {
    val values = List(NoEmitterCategoryA, NoEmitterCategoryB, NoEmitterCategoryC, NoEmitterCategoryD,
      Light, Small, Large, HighVortexLarge, Heavy, HighPerformance, Rotocraft,
      Glider, LighterThanAir, Parachutist, UltraLight, UAV, Space,
      EmergencyVehicle, ServiceVehicle, PointObstacle, ClusterObstacle, LineObstacle)

    def apply(tc: Int, category: Int): EmitterCategory = values.
      filter(e => e.tc == tc && e.category == category).headOption.getOrElse(ReservedEmitterCategory(tc, category))
  }

  sealed trait Message {
    def ts: Timestamp
  }
  sealed trait Identified extends Message{
    def id: AircraftAddress
  }
  case class AirbornePositionMessage(ts: Timestamp, id: AircraftAddress, ss: SurveillanceStatus, altitude: Altitude,
                                     sync: TimeSynchronization, cpr: CompactPositionRepresentation,
                                     containment: HorizontalContainmentRadius) extends Message with Identified
  case class SurfacePositionMessage(ts: Timestamp, id: AircraftAddress, sync: TimeSynchronization,
                                    cpr: CompactPositionRepresentation, containment: HorizontalContainmentRadius) extends Message with Identified
  case class AirborneOperationalStatusMessage(ts: Timestamp, id: AircraftAddress, nicA: NicA) extends Message with Identified
  case class SurfaceOperationalStatusMessage(ts: Timestamp, id: AircraftAddress, nicA: NicA) extends Message with Identified
  case class IdentificationMessage(ts: Timestamp, id: AircraftAddress, category: EmitterCategory, callsign: String) extends Message with Identified

  case class Unknown(ts: Timestamp, bytes: ByteVector) extends Message

  sealed trait CompactPositionRepresentation {
    def lat: Int
    def lon: Int
    def odd: Boolean
  }
  case class EvenCompactPositionRepresentation(lat: Int, lon: Int) extends CompactPositionRepresentation {
    val odd = false
  }
  case class OddCompactPositionRepresentation(lat: Int, lon: Int) extends CompactPositionRepresentation {
    val odd = true
  }

}