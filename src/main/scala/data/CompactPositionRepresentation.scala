package data

import data.Datum._

case class Position(lat: Double, lon: Double)

/**
 *
 * Decoding, local and global position is ported from https://github.com/openskynetwork/java-adsb/blob/master/src/main/java/org/opensky/libadsb/msgs/AirbornePositionMsg.java
 * org.opensky.libadsb is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  org.opensky.libadsb is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with org.opensky.libadsb.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Encoding is ported from https://github.com/bistromath/gr-air-modes/blob/master/python/cpr.py
 * # Copyright 2010, 2012 Nick Foster
 * #
 * # This file is part of gr-air-modes
 * #
 * # gr-air-modes is free software; you can redistribute it and/or modify
 * # it under the terms of the GNU General Public License as published by
 * # the Free Software Foundation; either version 3, or (at your option)
 * # any later version.
 * #
 * # gr-air-modes is distributed in the hope that it will be useful,
 * # but WITHOUT ANY WARRANTY; without even the implied warranty of
 * # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * # GNU General Public License for more details.
 * #
 * # You should have received a copy of the GNU General Public License
 * # along with gr-air-modes; see the file COPYING.  If not, write to
 * # the Free Software Foundation, Inc., 51 Franklin Street,
 * # Boston, MA 02110-1301, USA.
 */
object AirborneCPR {
  val Dlat0 = 360.0 / 60.0;
  val Dlat1 = 360.0 / 59.0;

  val const = (1 << 17).asInstanceOf[Double]

  def mod(a: Double, b: Double) = ((a % b) + b) % b;
  def NL(Rlat: Double): Double = {
    if (Rlat == 0) return 59;
    else if (Math.abs(Rlat) == 87) return 2;
    else if (Math.abs(Rlat) > 87) return 1;

    val tmp = 1 - (1 - Math.cos(Math.PI / (2.0 * 15.0))) / Math.pow(Math.cos(Math.PI / 180.0 * Math.abs(Rlat)), 2);
    Math.floor(2 * Math.PI / Math.acos(tmp))
  }

  def global(m1: CompactPositionRepresentation, m2: CompactPositionRepresentation): Option[Position] = {
    if (m1.odd == m2.odd)
      None
    else {
      val (even: CompactPositionRepresentation, odd: CompactPositionRepresentation) = m1.odd match {
        case false => (m1, m2)
        case true  => (m2, m1)
      }

      val j = Math.floor((59.0 * even.lat - 60.0 * odd.lat) / const + 0.5);

      var Rlat0 = Dlat0 * (mod(j, 60) + even.lat / const);
      var Rlat1 = Dlat1 * (mod(j, 59) + odd.lat / const);

      if (Rlat0 >= 270 && Rlat0 <= 360) Rlat0 -= 360;
      if (Rlat1 >= 270 && Rlat1 <= 360) Rlat1 -= 360;

      if (Rlat0 <= -270 && Rlat0 >= -360) Rlat0 += 360;
      if (Rlat1 <= -270 && Rlat1 >= -360) Rlat1 += 360;

      if (NL(Rlat0) != NL(Rlat1))
        return None

      val Dlon0 = 360.0 / Math.max(1.0, NL(Rlat0));
      val Dlon1 = 360.0 / Math.max(1.0, NL(Rlat1) - 1);

      val NL_helper = NL(if (m2.odd) Rlat1 else Rlat0); // assuming m2  is the newer message
      val m = Math.floor((even.lon * (NL_helper - 1) - odd.lon * NL_helper) / const + 0.5);

      var Rlon0 = Dlon0 * (mod(m, Math.max(1.0, NL(Rlat0))) + even.lon / const);
      var Rlon1 = Dlon1 * (mod(m, Math.max(1.0, NL(Rlat1) - 1)) + odd.lon / const);

      if (Rlon0 < -180 && Rlon0 > -360) Rlon0 += 360;
      if (Rlon1 < -180 && Rlon1 > -360) Rlon1 += 360;
      if (Rlon0 > 180 && Rlon0 < 360) Rlon0 -= 360;
      if (Rlon1 > 180 && Rlon1 < 360) Rlon1 -= 360;

      Some(if (m1.odd) Position(Rlat1, Rlon1) else Position(Rlat0, Rlon0))
    }
  }

  def local(p: CompactPositionRepresentation, ref: Position): Position = {
    val Dlat = if (p.odd) 360.0 / 59.0 else 360.0 / 60.0;

    val j = Math.floor(ref.lat / Dlat) + Math.floor(0.5 + (mod(ref.lat, Dlat)) / Dlat - p.lat / (((1 << 17).asInstanceOf[Double])))

    val Rlat = Dlat * (j + p.lat / ((1 << 17).asInstanceOf[Double]));

    val Dlon = 360.0 / Math.max(1.0, NL(Rlat) - (if (p.odd) 1.0 else 0.0));

    val m =
      Math.floor(ref.lon / Dlon) +
        Math.floor(0.5 + (mod(ref.lon, Dlon)) / Dlon - (p.lon).asInstanceOf[Float] / ((1 << 17).asInstanceOf[Double]));

    val Rlon = Dlon * (m + p.lon / ((1 << 17).asInstanceOf[Double]));

    Position(Rlat, Rlon)

  }

  def apply(lat: Double, lon: Double, odd: Boolean): CompactPositionRepresentation = {
    val ctype = if (odd) 1 else 0
    val nz = 4 * 15 - ctype
    val dlati = if (nz == 0) 360.0 else 360.0 / nz
    val nl = if (Math.abs(lat) >= 87.0)
      1.0
    else
      Math.floor((2.0 * Math.PI) * Math.pow(Math.acos(1.0 - (1.0 - Math.cos(Math.PI / (2.0 * 15))) / Math.pow(Math.cos((Math.PI / 180.0) * Math.abs(lat)), 2)), -1))

    val dloni = 360.0 / Math.max(nl - ctype, 1)

    val yz = Math.floor(const * ((lat % dlati) / dlati) + 0.5)
    val rlat = dlati * ((yz / const) + math.floor(lat / dlati))

    val xz = Math.floor(const * ((lon % dloni) / dloni) + 0.5)

    val x = (yz).asInstanceOf[Int] & ((1 << 17) - 1)
    val y = (xz).asInstanceOf[Int] & ((1 << 17) - 1)
    if (odd) OddCompactPositionRepresentation(x, y) else EvenCompactPositionRepresentation(x, y)
  }
}