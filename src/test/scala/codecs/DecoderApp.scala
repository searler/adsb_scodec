package codecs

import scodec.bits.BitVector
import data.Datum.Timestamp


import scala.io.Source

object DecoderApp extends App {

  import scodec.Decoder

  val codecs = new Codecs({
    var count = 0
    () => { count += 1; Timestamp(count) }
  })

  val lines = Source.fromInputStream(classOf[App].getResourceAsStream("/messages.txt")).getLines()

  lines.map { l => BitVector.fromHex(l).get }.map { bv => scodec.Decoder.decode(bv)(codecs.msg).require.value }.foreach { println }

}