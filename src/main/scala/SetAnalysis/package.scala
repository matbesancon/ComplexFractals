/**
  * Created by mbesancon on 8/6/16.
  */

import annotation.tailrec;
import Math.sqrt;

import ComplexNumbers._;

package object SetAnalysis {
  def simPoly(c:Complex): (Complex=>Complex) = {
    def f(z:Complex): Complex = z*z+c
    f
  }

  def diverge(z: Complex, c: Complex): Boolean = {
    def criticalRadius(c: Complex): Double = (1+sqrt(1+4*c.radius))/2
    z.radius>criticalRadius(c)
  }

  @tailrec def iterMap(c: Complex, z: Complex, niter: Int): Complex =
    if (niter==0) z else iterMap(c, simPoly(c)(z), niter-1)

  /** Builds a black and white Julia map: Julia set has value 1
    * other elements have 0
    * @param c Complex: polynomial function parameter f(z) = z*z + c
    * @param z current complex point
    * @param niter number of iterations already performed
    * @param nitermax number of iterations at which the computation stops and considers the series never diverges
    */
  @tailrec def binaryJulia(c: Complex, z: Complex, niter: Int = 0, nitermax: Int = 5000): Int =
    if (niter>=nitermax) 1
    else if (diverge(z, c)) 0
    else binaryJulia(c, simPoly(c)(z), niter+1,nitermax)

  /** Builds a colored Julia map: Julia set has value -1
    * other elements have how many iterations before diverging
    * @param c Complex: polynomial function parameter f(z) = z*z + c
    * @param z current complex point
    * @param niter number of iterations already performed
    * @param nitermax number of iterations at which the computation stops and considers the series never diverges
    * @return integer to interpret as a color in the map, -1 is usually black, higher colors should be lighter
    */
  @tailrec def colorJulia(c: Complex, z: Complex, niter: Int = 0, nitermax: Int = 5000): Int =
    if (niter>=nitermax) -1
    else if (diverge(z, c)) niter
    else {
      val z1 = simPoly(c)(z)
      colorJulia(c, z1, niter+1,nitermax)
    }

  @tailrec def binaryMandel(c: Complex, niter: Int = 0, nitermax: Int = 5000, z: Complex = new CartesianComplex(0,0)): Int =
    if (niter>=nitermax) 1
    else if (z.radius>2) 0
    else binaryMandel(c, niter+1, nitermax, simPoly(c)(z))

  @tailrec def colorMandel(c: Complex, niter: Int = 0, nitermax: Int = 5000, z: Complex = new CartesianComplex(0,0)): Int =
    if (niter>=nitermax) -1
    else if (z.radius>2) niter
    else colorMandel(c, niter+1, nitermax, simPoly(c)(z))
}
