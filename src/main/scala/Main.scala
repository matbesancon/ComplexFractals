/**
  * Created by mbesancon on 8/6/16.
  */
import java.io.File

import MatrixGenerator._;

object MainClass {

  def writeToFile(p: String, s: String): Unit = {
    val pw = new java.io.PrintWriter(new File(p))
    try pw.write(s) finally pw.close()
  }

  def main(args: Array[String]): Unit = {
    val testMandel = genMandel(1200*5,800*5,-2,-1,0.0025/3,0.0025/3,false,10000)
//    val testMandelColor = genMandel(1200,800,-2,-1,0.00025,0.0025,true,10000)
//    println(testMandelColor)
//    println("------------")
//    println(testMandelColor.map{_.max}.max)
    val stringMandel = testMandel map {_.mkString(",")} mkString("\n")
    writeToFile("mandelBlack.csv",stringMandel)
  }
}
