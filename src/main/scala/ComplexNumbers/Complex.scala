package ComplexNumbers

import Math.sqrt;
import Math.atan2;
import Math.cos;
import Math.sin;

import BasicFunctions.nearZero;

/**
  * Created by mbesancon on 8/6/16.
  */

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
  def isNull: Boolean = nearZero(real) && nearZero(imaginary)

  def /(that: Complex) = if (that.isNull) None else {
    that.inverse match {
      case None => None
      case Some(z) => Some(this * z)
    }
  }

  // abstract methods, implemented in inherited
  def *(that: Complex): Complex
  def *(that: Double): Complex
  def /(that: Double): Option[Complex]
  def conjugate: Complex
  def inverse: Option[Complex]
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

  def inverse = if(this.isNull) None else Some(
    new CartesianComplex(
      this.real/(this.real*this.real+this.imaginary*this.imaginary)
    )
  )

  def /(that: Double) = if (!nearZero(that)) Some(this*(1/that)) else None
  def conjugate: Complex = new CartesianComplex(this.real,-this.imaginary)
}

class PolarComplex(givenRadius: Double, givenArg: Double = 0) extends Complex{
  val real = givenRadius*cos(givenArg)
  val imaginary = givenRadius*sin(givenArg)
  val radius = givenRadius
  val argument = givenArg

  def *(that: Complex) =
    new PolarComplex(this.radius*that.radius,this.real*that.imaginary-this.imaginary*that.real)

  def *(that: Double) =
    new CartesianComplex(this.real*that,this.imaginary*that)

  def /(that: Double) = if (!nearZero(that)) Some(this*(1/that)) else None
  def conjugate: Complex = new CartesianComplex(this.real,-this.imaginary)
  def inverse = if(this.isNull) None else Some(
    new PolarComplex(1/this.radius,1/this.argument)
  )
}
