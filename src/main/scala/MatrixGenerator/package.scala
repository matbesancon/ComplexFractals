/**
  * Created by mbesancon on 8/6/16.
  */

import ComplexNumbers._
import SetAnalysis._

import scala.collection.parallel.immutable.{ParSeq, ParVector};

package object MatrixGenerator {

  def genMandel(width: Int, height: Int, x0: Double, y0: Double, scaleHoriz: Double, scaleVerti: Double, colored: Boolean, nitermax: Int = 5000): ParSeq[ParSeq[Int]] = {
    (0 until height).par map {i=>
      (0 until width).par map {j=>
        if(colored) colorMandel(new CartesianComplex(x0+i*scaleHoriz,y0+j*scaleVerti),0,nitermax)
        else binaryMandel(new CartesianComplex(x0+i*scaleHoriz,y0+j*scaleVerti),0,nitermax)
      }
    }
  }

  def genJulia(width: Int, height: Int, x0: Double, y0: Double, scaleHoriz: Double, scaleVerti: Double, colored: Boolean, c: Complex, nitermax: Int = 5000): ParSeq[ParSeq[Int]] = {
    (0 until height).par map {i=>
      (0 until width).par map {j=>
        if(colored) colorJulia(c, new CartesianComplex(x0+i*scaleHoriz,y0+j*scaleVerti),0,nitermax)
        else binaryMandel(new CartesianComplex(x0+i*scaleHoriz,y0+j*scaleVerti),0,nitermax)
      }
    }
  }

}
