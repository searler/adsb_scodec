package operations

import scodec.bits.BitVector
import scala.io.Source
import codecs.Codecs
import data.Datum._
import scala.annotation.tailrec

object CurrentAirborneStateApp extends App {
  import scodec.Decoder
  import scala.concurrent.duration._

  val codecs = new Codecs({
    var count = 0
    () => {
      count += 1
      Timestamp(count)
    }
  })

  val pv = CurrentAirborneState.processValues(Duration(1, SECONDS), Duration(15, SECONDS)) _

  val v = Source.fromInputStream(classOf[App].getResourceAsStream("/messages.txt"))
    .getLines()
    .map { l => BitVector.fromHex(l).get }
    .map { bv => scodec.Decoder.decode(bv)(codecs.msg).require.value }
    .collect { case ided: Identified => ided }
    .toSeq
    .groupBy { _.id }
    .mapValues(s => process(s).reverse)

  v.mapValues(_.mkString("\n")).foreach { println }

  @tailrec def process(values: Seq[Message],
                       state: Option[CurrentAirborneState] = None,
                       accum: List[Option[CurrentAirborneState]] = Nil): List[Option[CurrentAirborneState]] =
    values match {
      case Seq() => accum
      case _ =>
        val newState = pv(Seq(values.head), state)
        process(values.tail, newState, newState :: accum)
    }

}