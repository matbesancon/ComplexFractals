/**
  * Created by mbesancon on 06.08.16.
  */

import Math.sqrt;
import Math.atan2;
import Math.cos;
import Math.sin;
import Math.abs;

abstract class Complex {
  val real: Double
  val imaginary: Double
  val radius: Double
  val argument: Double
  def argInPi = argument/3.1416
  def +(that: Complex): Complex =
    new CartesianComplex(this.real+that.real,this.imaginary+that.imaginary)
  def -(that: Complex): Complex =
    new CartesianComplex(this.real-that.real,this.imaginary-that.imaginary)
  def +(that: Double): Complex =
    new CartesianComplex(this.real+that,this.imaginary)
  def -(that: Double): Complex = this+(-that)
//    new CartesianComplex(this.real-that,this.imaginary)
  def *(that: Complex): Complex
  def *(that: Double): Complex
  def /(that: Complex): Option[Complex]
  def /(that: Double): Option[Complex]
  def conjugate: Complex
}

class CartesianComplex(realPart: Double, imagPart: Double = 0) extends Complex{
  val real = realPart
  val imaginary = imagPart
  val radius = sqrt(realPart*realPart + imagPart*imagPart)
  val argument = atan2(imagPart,realPart)
  def *(that: Complex) =
    new CartesianComplex(this.real*that.real-this.imaginary*that.imaginary,this.real*that.imaginary-this.imaginary*that.real)
  def *(that: Double) =
    new CartesianComplex(this.real*that,this.imaginary*that)
  def /(that: Complex)

  // TODO: continue here 
  def /(that: Double) = if (that!=0) Some(this*(1/that)) else None
  def conjugate: Complex = new CartesianComplex(this.real,-this.imaginary)
}

class PolarComplex(givenRadius: Double, givenArg: Double = 0) extends Complex{
  val real = givenRadius*cos(givenArg)
  val imaginary = givenRadius*sin(givenArg)
  val radius = givenRadius
  val argument = givenArg
}
