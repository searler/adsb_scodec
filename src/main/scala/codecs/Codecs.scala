package codecs

import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._
import data.Datum._
import scodec.codecs.IntCodec
import scodec.bits.ByteOrdering
import scodec.codecs.IntCodec
import scodec.Decoder
import scodec.Attempt
import scodec.DecodeResult
import scodec.Err
import scodec.Encoder

class Codecs(tsSource: () => Timestamp) {

  final class PeekCodec[T](target: Codec[T]) extends Codec[T] {

    def sizeBound = target.sizeBound

    def encode(value: T) = target.encode(value)

    def decode(buffer: BitVector) =
      target.decode(buffer) match {
        case Attempt.Successful(DecodeResult(value, rest)) => Attempt.successful(DecodeResult(value, buffer))
        case f: Attempt.Failure                            => f
      }
  }

  def peek[T](target: Codec[T]) = new PeekCodec(target)

  class RangeDiscriminatorCodec[T](start: Int, end: Int, tag: Codec[Int], child: Codec[Int ~ T]) extends Codec[Int ~ T] {
    override def sizeBound = tag.sizeBound | child.sizeBound
    override def encode(value: Int ~ T) = value match {
      case t ~ _ if t >= start && t <= end => child.encode(value)
      case _                               => Attempt.failure(Err("out of range"))
    }
    override def decode(bits: BitVector) = tag.decode(bits) match {
      case Attempt.Successful(DecodeResult(value, remainder)) =>
        if (value >= start && value <= end)
          child.decode(remainder)
        else
          Attempt.failure(Err("out of range"))
      case Attempt.Failure(err) => Attempt.failure(err)
    }
  }

  def tc[T](start: Int, end: Int, child: Codec[Int ~ T]) = new RangeDiscriminatorCodec(start, end, peek(uint5), child)

  val uint5: Codec[Int] = uint(5)
  val uint6: Codec[Int] = uint(6)
  val uint7: Codec[Int] = uint(7)
  val uint3: Codec[Int] = uint(3)
  val uint24: Codec[Int] = uint(24)
  val uint11: Codec[Int] = uint(11)
  val uint17: Codec[Int] = uint(17)

  val addr: Codec[AircraftAddress] = uint24.xmap(
    AircraftAddress(_),
    _.value)

  val parity = ignore(24)

  def c5(c: Int) = constant(BitVector.fromInt(c, 5))
  def c3(c: Int) = constant(BitVector.fromInt(c, 3))
  def c2(c: Int) = constant(BitVector.fromInt(c, 2))
  def c1(c: Int) = constant(BitVector.fromInt(c, 1))

  val survStatus: Codec[SurveillanceStatus] = mappedEnum(uint(2), (No_Condition_Information -> 0),
    (Permanent_Alert -> 1),
    (Temporary_Alert -> 2),
    (SPI_Condition -> 3))

  val sync: Codec[TimeSynchronization] = mappedEnum(uint(1), (Unsynchronized -> 0), (Synchronized -> 1))

  val nicA: Codec[NicA] = uint(1).xmap(NicA(_), _.value)
  val nicB: Codec[NicB] = uint(1).xmap(NicB(_), _.value)

  val latitude: Codec[Int] = uint17
  val longitude: Codec[Int] = uint17

  val capabilities = ignore(3)

  val adsbDF = c5(17)

  val altitudeValue: Codec[Int] = (bits(7) ~ bool ~ bits(4)).xmap(
    p => {
      val first ~ qbit ~ last = p
      ((Decoder.decode(first ++ last)(uint11).require.value) * (if (qbit) 25 else 100)) - 1000
    },
    v => {
      val bits = Encoder.encode((v + 1000) / 25)(uint11).require
      bits.take(7) ~ true ~ bits.takeRight(4)
    })

  val barometric: Codec[Altitude] = altitudeValue.xmap(
    p => if (p == 0) UnknownAltitude else BarometricAltitude(p),
    _.feet.getOrElse(0))

  val gnss: Codec[Altitude] = altitudeValue.xmap(
    p => if (p == 0) UnknownAltitude else GNSSAltitude(p),
    _.feet.getOrElse(0))

  val cpr: Codec[CompactPositionRepresentation] = (uint(1) ~ latitude ~ longitude).xmap(
    p => {
      val odd ~ lat ~ lon = p
      odd match {
        case 1 => OddCompactPositionRepresentation(lat, lon)
        case 0 => EvenCompactPositionRepresentation(lat, lon)
      }
    },
    _ match {
      case OddCompactPositionRepresentation(lat, lon)  => 1 ~ lat ~ lon
      case EvenCompactPositionRepresentation(lat, lon) => 0 ~ lat ~ lon
    })

  def core(altitude: Codec[Altitude]) = (uint5 ~ (survStatus ~ nicB ~ altitude ~ sync ~ cpr))

  val barometricAirbornePosition: Codec[AirbornePositionMessage] = (addr ~ tc(9, 18, core(barometric))).xmap(
    p => {
      val address ~ (tc ~ (surv ~ nb ~ alt ~ sync ~ rep)) = p
      AirbornePositionMessage(tsSource(), address, surv, alt, sync, rep, DerivedHorizontalContainmentRadius(nb, TypeCode(tc)))
    },
    v => {
      val (nicB, tc) = v.containment match {
        case DerivedHorizontalContainmentRadius(nicB, tc) => (nicB, tc.value)
        case _ => (NicB(0), -1)
      }
      v.id ~ (tc ~ (v.ss ~ nicB ~ v.altitude ~ v.sync ~ v.cpr))
    })

  val gnssAirbornePosition: Codec[AirbornePositionMessage] = (addr ~ tc(20, 22, core(gnss))).xmap(
    p => {
      val address ~ (tc ~ (surv ~ nb ~ alt ~ sync ~ rep)) = p
      AirbornePositionMessage(tsSource(), address, surv, alt, sync, rep, KnownHorizontalContainmentRadius(TypeCode(tc)))
    },
    v => {
      val tc = KnownHorizontalContainmentRadius.typeCode(v.containment).value
      v.id ~ (tc ~ (v.ss ~ NicB(0) ~ v.altitude ~ v.sync ~ v.cpr))
    })

  val surfacePosition: Codec[SurfacePositionMessage] = (addr ~ tc(5, 8, uint5 ~ (uint7 ~ bool ~ uint7 ~ sync ~ cpr))).xmap(
    p => {
      val address ~ (tc ~ (_ ~ _ ~ _ ~ sync ~ rep)) = p

      SurfacePositionMessage(tsSource(), address, sync, rep, SurfaceHorizontalContainmentRadius(TypeCode(tc)))
    },
    v => {
      val tc = v.containment match {
        case SurfaceHorizontalContainmentRadius(tc) => tc.value
        case _                                      => -1
      }
      v.id ~ (tc ~ (0 ~ false ~ 0 ~ v.sync ~ v.cpr))
    })

  val airborneOperStatus = (uint5 ~ (c3(0) ~> (ignore(35) ~> nicA)) <~ ignore(12))

  val airborneOperationalStatus: Codec[AirborneOperationalStatusMessage] = (addr ~ tc(31, 31, airborneOperStatus)).xmap(
    p => {
      val address ~ (tc ~ na) = p
      AirborneOperationalStatusMessage(tsSource(), address, na)
    },
    v => v.id ~ (31 ~ v.nicA))

  val surfaceOperStatus = (uint5 ~ (c3(1) ~> (ignore(35) ~> nicA)) <~ ignore(12))

  val surfaceOperationalStatus: Codec[SurfaceOperationalStatusMessage] = (addr ~ tc(31, 31, surfaceOperStatus)).xmap(
    p => {
      val address ~ (tc ~ na) = p
      SurfaceOperationalStatusMessage(tsSource(), address, na)
    },
    v => v.id ~ (31 ~ v.nicA))

  val nacv :Codec[VelocityNAC] = mappedEnum(uint3,
    VelocityNAC0 -> 0 , VelocityNAC1 -> 1, VelocityNAC2 -> 2, VelocityNAC3 ->3, VelocityNAC4 ->4)
  val verticalRate:Codec[VerticalRate] = (bool(1) ~ bool(1) ~int(9)).xmap (
     _ match {
       case (true ~ d) ~ r => GeometricVerticalRate(if(d) -r else r)
       case (false ~ d) ~r =>    BarometricVerticalRate(if(d) -r else r)

     },
    _ match {
      case GeometricVerticalRate(r) => (true  ~ (r< 0) )~ r
      case BarometricVerticalRate(r) => (false  ~ (r< 0))~ r
    }
  )

  val gvel:Codec[Int] = (bool(1) ~ uint(10)).xmap (
    _ match {
      case true ~ v => -1 * (v-1)
      case false ~ v => v -1
    },
    v  => if(v > 0) false ~( v + 1) else true ~ ((-v) + 1)
  )
  val x = (addr ~ ((c5(19) ~> c3(1)) ~> bool(1) ~ (ignore(1) ~> nacv) ~ gvel ~ gvel ~ verticalRate ~ (ignore(2) ~> int(8)) ))
  val subsonicGroundSpeed : Codec[SubsonicGroundVelocityMessage] = x.xmap (
    p => {
      val  (addr ~ (((((icf ~ nac) ~ x) ~ y) ~ vr) ~bd)) = p

      val h = math.atan2(x,y)* (360.0/ (2.0*math.Pi))
      SubsonicGroundVelocityMessage(tsSource(),addr,icf,nac,
        if(h<0) h + 360 else h,
        math.sqrt(x*x + y*y),
        vr, bd )
    },
    v => null
  )

  def call: Codec[String] = {
    val ais_charset = "?ABCDEFGHIJKLMNOPQRSTUVWXYZ????? ???????????????0123456789??????".toVector

    listOfN(provide(8), uint6).xmap(
      _.map(ais_charset.apply).mkString.trim,
      _.toList.map(ais_charset.indexOf[Char]) padTo (8, 32))
  }

  val ident = (uint5 ~ (uint3 ~ call))

  val identification: Codec[IdentificationMessage] = (addr ~ tc(1, 4, ident)).xmap(
    p => {
      val address ~ (tc ~ (em ~ cs)) = p
      IdentificationMessage(tsSource(), address, EmitterCategory(tc, em), cs)
    },
    v => v.id ~ (v.category.tc ~ (v.category.category ~ v.callsign)))

  val nullParser: Codec[Unknown] = bits(112).xmap(
    p => Unknown(tsSource(), p.bytes),
    _.bytes.bits)

  val msg = choice(adsbDF ~> capabilities ~> choice(barometricAirbornePosition.upcast[Message],
    gnssAirbornePosition.upcast[Message],
    subsonicGroundSpeed.upcast[Message],
    surfacePosition.upcast[Message],
    airborneOperationalStatus.upcast[Message],
    surfaceOperationalStatus.upcast[Message],
    identification.upcast[Message]) <~ parity,
    nullParser.upcast[Message])

}