package basics
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer
import swiftvis2.plotting._

case class TempRow(day: Int, doy: Int, month: Int, year: Int, precip: Double, tave: Double, tmax:
  Double, tmin: Double)

object SATemps {

  def parseLine(line: String): TempRow = {
      val p = line.split(",")
      TempRow(p(0).toInt, p(1).toInt, p(2).toInt, p(4).toInt, p(5).toDouble, p(6).toDouble,
        p(7).toDouble, p(8).toDouble)
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("/users/mlewis/CSCI3395-F19/InClassBD/data/SanAntonioTemps.csv")
    val lines = source.getLines()
    val data = lines.drop(2).map(parseLine).toArray

    // data.take(5).foreach(println)

    // val highestTemp = data.maxBy(_.tmax)
    // val highestTemp2 = data.reduce((d1, d2) => if(d1.tmax > d2.tmax) d1 else d2)
    // println(highestTemp.month, highestTemp.day, highestTemp.year)
    // println(highestTemp.month, highestTemp.day, highestTemp.year)

    // val mostPrecip = data.maxBy(_.precip)
    // println(mostPrecip.month, mostPrecip.day, mostPrecip.year)

    val rainyDayCount = data.count(day => day.precip > 1)
    // println(rainyDayCount + "/" + data.size)

    //Average high temp for "rainy days"
    val rainyDaysTempSum = data.filter(day => day.precip > 1).foldLeft(0.0){ (acc, i) => acc + i.tmax } 
    println(rainyDaysTempSum / rainyDayCount.toDouble)

    //ONLY WITH A FOLD
    val (rainySum, rainyCount) = data.foldLeft((0.0, 0)){ case ((sum, cnt), day) =>
      if (day.precip >= 1) (sum + day.precip, cnt + 1) else (sum, cnt)
    }

    //Average high temp monthly
    def monthTempAvg(monthGroup:Array[TempRow]) :Double = {
      val tempSum = monthGroup.foldLeft(0.0){ (acc, i) => acc + i.tmax }
      return tempSum / monthGroup.size.toDouble
    }

    val monthGroups: Map[Int, Array[TempRow]] = data.groupBy(_.month)
    monthGroups foreach { case (month, monthGroup) =>
      println("The average high temperature in month " + month + " is " + monthTempAvg(monthGroup))
    }

    //Average precipitation monthly
    def monthPrecipAvg(monthGroup:Array[TempRow]) :Double = {
      val precipSum = monthGroup.foldLeft(0.0){ (acc, i) => acc + i.precip }
      return precipSum / monthGroup.size.toDouble
    }

    monthGroups foreach { case (month, monthGroup) =>
      println("The average precipitation in month " + month + " is " + monthPrecipAvg(monthGroup))
    }


    //Median precipitation monthly
    def monthPrecipMed(monthGroup:Array[TempRow]) :Double = {
      val sorted = monthGroup.sortBy(_.precip)
      val s = monthGroup.size
      if (s % 2 != 0) return monthGroup(s/2).precip.toDouble else return ((monthGroup((s-1)/2).precip + monthGroup(s/2).precip).toDouble / 2.0)
    }

    monthGroups foreach { case (month, monthGroup) => 
      println("The median precipitation in month " + month + " is " + monthPrecipMed(monthGroup))
    }

    val cg = ColorGradient(1946.0 -> RedARGB, 1975.0 -> BlueARGB, 2014.0 -> GreenARGB)
    val sizes = data.map(_.precip * 2 + 2)
    val tempByDayPlot = Plot.simple(
      ScatterStyle(data.map(_.doy), data.map(_.tave), symbolWidth = sizes, symbolHeight = sizes, colors = cg(data.map(_.year))), 
      "SA Temps", "Day of Year", "Temp")
    SwingRenderer(tempByDayPlot, 800, 800, true)
  }
}