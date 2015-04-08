package operations

import data.Datum._
import data.Position
import data.AirborneCPR

case class CurrentAirborneState(lastUpdate: Timestamp,
                                callsign: Option[String],
                                position: Option[Position],
                                lastFix: Option[CompactPositionRepresentation])

object CurrentAirborneState {
  import scala.concurrent.duration._

  def update = processValues(Duration(5, MINUTES), Duration(15, SECONDS))_

  def processValues(maxAge: Duration, deltaFix: Duration)(newValues: Seq[Message], current: Option[CurrentAirborneState]): Option[CurrentAirborneState] = {
    current match {
      case None =>
        processValues(maxAge, deltaFix)(newValues, Some(CurrentAirborneState(Timestamp(0), None, None, None)))
      case c @ Some(cur) if newValues.isEmpty =>
        if (cur.lastUpdate olderThan maxAge) None else c
      case Some(cur) =>
        Some(newValues.sortBy { _.ts.value }.filter(_.ts moreRecentThan cur.lastUpdate).foldLeft(cur)(process(deltaFix)))
    }

  }
  private[this] def process(deltaFix: Duration)(state: CurrentAirborneState, m: Message): CurrentAirborneState = {
    def local(cpr: CompactPositionRepresentation) = state.position match {
      case None    => state.copy(lastUpdate = m.ts, lastFix = Some(cpr))
      case Some(p) => state.copy(lastUpdate = m.ts, lastFix = Some(cpr), position = Some(AirborneCPR.local(cpr, p)))
    }
    m match {
      case cs: IdentificationMessage          => state.copy(lastUpdate = cs.ts, callsign = Some(cs.callsign))
      case s: SurfaceOperationalStatusMessage => state.copy(lastUpdate = s.ts, lastFix = None)
      case s: SurfacePositionMessage          => state.copy(lastUpdate = s.ts, lastFix = None)
      case a: AirbornePositionMessage if state.lastUpdate.noOlderThan(a.ts, deltaFix) => state.lastFix match {
        case Some(lastFix) => AirborneCPR.global(lastFix, a.cpr) match {
          case None        => local(a.cpr)
          case s @ Some(_) => state.copy(lastUpdate = a.ts, lastFix = Some(a.cpr), position = s)
        }
        case _ => local(a.cpr)
      }
      case _ => state.copy(lastUpdate = m.ts)
    }
  }
}