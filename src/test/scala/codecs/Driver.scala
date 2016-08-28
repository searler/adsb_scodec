package codecs

import java.io.FileInputStream
import scodec.bits.ByteVector

object Driver extends App {

  import codecs.Codecs

  import data.Datum._

  import scodec.bits._

  val fis = new FileInputStream("/home/rsearle/work/adsb_scodec/src/test/resources/messages.txt")
  val bytes = new Array[Byte](fis.available)
  fis.read(bytes)
  fis.close

  val bv = ByteVector.fromHex(new String(bytes)).get

   val cs = new Codecs(() => data.Datum.Timestamp(0))

  val results = scodec.Decoder.decodeCollect[List, Message](cs.msg, None)(bv.bits).require.value
  println(results.mkString("\n"))
}