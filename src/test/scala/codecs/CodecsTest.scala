package codecs

import org.junit.Assert.assertEquals
import org.junit.Test
import scodec.Decoder

class CodecsTest {

  import scodec.bits._

  import data.Datum._

  val codecs = new Codecs(() => Timestamp(0))

  def zeroTS = () => Timestamp(0)

  val decoder = CodecTest.decode(codecs.msg) _
  val encoder = CodecTest.encode(codecs.msg) _
  val round = CodecTest.round(codecs.msg) _

  @Test
  def groundSpeed(): Unit = {
    val msg = SubsonicGroundVelocityMessage(Timestamp(0), AircraftAddress("485020"), false, VelocityNAC0,
      182.8803775528476, 159.20113064925135,
      BarometricVerticalRate(-14), 23)
    assertEquals(msg,
      decoder(hex"8D485020994409940838175B284F"))

    round(msg)
  }

  @Test
  def callsign {
    assertEquals("c01c42820820", CodecTest.encode(codecs.call)("0A1B"))
    CodecTest.round(codecs.call)("0A")
    CodecTest.round(codecs.call)("0 A")
  }

  @Test
  def altitudeTest {
    CodecTest.round(codecs.barometric)(BarometricAltitude(36000))
    assertEquals("b98", CodecTest.encode(codecs.barometric)(BarometricAltitude(36000)))
  }

  @Test
  def ssTest {
    CodecTest.round(codecs.survStatus)(No_Condition_Information)
  }

  @Test
  def nicBTest {
    CodecTest.round(codecs.nicB)(NicB(0))
  }

  @Test
  def encoding {
    val src = AirbornePositionMessage(Timestamp(0), AircraftAddress(12648430), No_Condition_Information, BarometricAltitude(36000), Unsynchronized, OddCompactPositionRepresentation(92249, 113957), DerivedHorizontalContainmentRadius(NicB(0), TypeCode(11)))
    assertEquals("c0ffee58b986d0b3bd25", CodecTest.encode(codecs.barometricAirbornePosition)(src))
    CodecTest.round(codecs.barometricAirbornePosition)(src)
  }

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
    val odd = hex"8dc0ffee58b986d0b3bd25000000"
    val even = hex"8dc0ffee58b9835693c897000000"

    val oddMsg = (decoder(odd).asInstanceOf[AirbornePositionMessage])
    val evenMsg = (decoder(even).asInstanceOf[AirbornePositionMessage])

    assertEquals(Some(185.2F), oddMsg.containment.meters(None))
    assertEquals(Some(185.2F), oddMsg.containment.meters(Some(NicA(0))))
    assertEquals(Some(185.2F), oddMsg.containment.meters(Some(NicA(1))))

    assertEquals(AirbornePositionMessage(Timestamp(0), AircraftAddress(12648430), No_Condition_Information, BarometricAltitude(36000), Unsynchronized, OddCompactPositionRepresentation(92249, 113957), DerivedHorizontalContainmentRadius(NicB(0), TypeCode(11))), oddMsg)
    assertEquals(AirbornePositionMessage(Timestamp(0), AircraftAddress(12648430), No_Condition_Information, BarometricAltitude(36000), Unsynchronized, EvenCompactPositionRepresentation(109385, 116887), DerivedHorizontalContainmentRadius(NicB(0), TypeCode(11))), evenMsg)

    round(AirbornePositionMessage(Timestamp(0), AircraftAddress(12648430), No_Condition_Information, BarometricAltitude(36000), Unsynchronized, OddCompactPositionRepresentation(92249, 113957), DerivedHorizontalContainmentRadius(NicB(0), TypeCode(11))))

  }

  @Test
  def gnssRound {
    round(AirbornePositionMessage(Timestamp(0), AircraftAddress(12648430), No_Condition_Information, GNSSAltitude(36000), Unsynchronized, OddCompactPositionRepresentation(92249, 113957), KnownHorizontalContainmentRadius(TypeCode(21))))
  }

  @Test
  def npiRound {
    round(Unknown(Timestamp(0), ByteVector(Array[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))))
  }

  @Test //TODO
  def surfacePosition {
    round(SurfacePositionMessage(Timestamp(0), AircraftAddress(7700555), Unsynchronized, OddCompactPositionRepresentation(92249, 113957), SurfaceHorizontalContainmentRadius(TypeCode(7))))
  }

  @Test
  def surfaceOperStatus {
    round(SurfaceOperationalStatusMessage(Timestamp(0), AircraftAddress(7700555), NicA(1)))
    assertEquals("8875804bf9000000001000000000", encoder(SurfaceOperationalStatusMessage(Timestamp(0), AircraftAddress(7700555), NicA(1))))
  }

  @Test
  def airborneOperStatus {
    round(AirborneOperationalStatusMessage(Timestamp(0), AircraftAddress(7700555), NicA(1)))
    assertEquals("8875804bf8000000001000000000", encoder(AirborneOperationalStatusMessage(Timestamp(0), AircraftAddress(7700555), NicA(1))))
  }

  @Test
  def identification {
    round(IdentificationMessage(Timestamp(0), AircraftAddress(7700555), Light, "CALL123"));
    assertEquals("8875804b210c130cc72ce0000000", encoder(IdentificationMessage(Timestamp(0), AircraftAddress(7700555), Light, "CALL123")))
  }

  /**
    * http://adsb-decode-guide.readthedocs.io/en/latest/identification.html
    */
  @Test
  def identificationExample {
    assertEquals(IdentificationMessage(Timestamp(0), AircraftAddress(0x4840D6), NoEmitterCategoryA, "KLM1023"), decoder(hex"8D4840D6202CC371C32CE0576098"))
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
    val one = hex"8D75804B580FF2CF7E9BA6F701D0"
    val two = hex"8D75804B580FF6B283EB7A157117"

    val oneMsg = (decoder(one).asInstanceOf[AirbornePositionMessage])
    val twoMsg = (decoder(two).asInstanceOf[AirbornePositionMessage])

    assertEquals(Some(185.2F), oneMsg.containment.meters(None))
    assertEquals(Some(185.2F), oneMsg.containment.meters(Some(NicA(0))))
    assertEquals(Some(185.2F), oneMsg.containment.meters(Some(NicA(1))))

    assertEquals(AirbornePositionMessage(Timestamp(0), AircraftAddress(7700555), No_Condition_Information, BarometricAltitude(2175), Unsynchronized, EvenCompactPositionRepresentation(92095, 39846), DerivedHorizontalContainmentRadius(NicB(0), TypeCode(11))), oneMsg)
    assertEquals(AirbornePositionMessage(Timestamp(0), AircraftAddress(7700555), No_Condition_Information, BarometricAltitude(2175), Unsynchronized, OddCompactPositionRepresentation(88385, 125818), DerivedHorizontalContainmentRadius(NicB(0), TypeCode(11))), twoMsg)

  }

  @Test
  def ignoreTest {
    val ignore = hex"8D000000000000CF7E9BA6F701D0"

    assertEquals(Unknown(Timestamp(0), ignore), decoder(ignore))
  }

  @Test
  def examples {
    val data = SubsonicGroundVelocityMessage(Timestamp(0), AircraftAddress("48CB15"), false, VelocityNAC2, 61.771223818630425, 431.2957222138889, BarometricVerticalRate(1), -103)
    val input = hex"8d48cb1599117d19a00499000000"
    assertEquals(data, decoder(input))

    round(data)
  }

  import scodec.Codec
  import org.junit.Assert.assertEquals

  object CodecTest {
    def encode[T](e: Codec[T])(v: T) = scodec.Encoder.encode(v)(e).map {
      _.toHex
    }.require

    def decode[T](e: Codec[T])(bv: ByteVector) = scodec.Decoder.decode(bv.bits)(e).require.value

    def round[T](e: Codec[T])(src: T) = {
      val bv = scodec.Encoder.encode(src)(e).require

      val decoded = scodec.Decoder.decode(bv)(e).require.value
      if (!src.equals(decoded)) {
        println("---------------------")
        println(src)
        println(decoded)
        println("---------------------")
      }
      assertEquals(src, decoded)
    }
  }

}