/**
  * Created by mbesancon on 06.08.16.
  */

import Math.abs;

package object BasicFunctions {
  def nearZero(d: Double,eps: Double = 0.0000000001): Boolean = if (abs(d) <= eps) true else false
}