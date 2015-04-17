package operations

import scodec.bits.BitVector
import scala.io.Source
import codecs.Codecs
import data.Datum._
import scala.annotation.tailrec

object DumpUnknownsApp extends App {
  import scodec.Decoder
  import scala.concurrent.duration._

  val codecs = new Codecs({
    var count = 0
    () => {
      count += 1
      Timestamp(count)
    }
  })

  import scodec.codecs._

  val extract = (uint(5) <~ ignore(27)) ~ uint(5)

  val v = Source.fromInputStream(classOf[App].getResourceAsStream("/messages.txt"))
    .getLines()
    .map { l => BitVector.fromHex(l).get }
    .map { bv => scodec.Decoder.decode(bv)(codecs.msg).require.value }
    .collect { case unknown: Unknown => scodec.Decoder.decode(unknown.bytes.bits)(extract).require.value }
    .toSeq
    .distinct

  v.foreach(println)

}