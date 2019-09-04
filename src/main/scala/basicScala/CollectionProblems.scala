package basicScala

//Define case classes for rows from the datasets

case class EduRow(location: String /*row(2)*/, year: Int /*row(3)*/, ageGroup: String /*row(5)*/,
 sex: String /*row(7)*/, metric: String /*row(8)*/, unit: String /*row(9)*/, mean: Double /*row(10)*/, 
 upper: Double /*row(11)*/, lower: Double /*row(12)*/)

object CollectionProblems {

  def parseEduLine(line: String): EduRow = {
    //Special case for , in quoted country name
    val p = line.split(",")
    EduRow(p(2), p(3).toInt, p(5), p(7), p(8), p(9), p(10).toDouble, 
      p(11).toDouble, p(12).toDouble)
  }
  
  def main(args: Array[String]): Unit = {
    val eduSource = scala.io.Source.fromFile("/users/mlewis/workspaceF18/CSCI3395-F18/data/BasicScala/IHME_GLOBAL_EDUCATIONAL_ATTAINMENT_1970_2015_Y2015M04D27.CSV")
    val eduLines = eduSource.getLines()
    val eduData = eduLines.drop(1).map(parseEduLine).toArray

    val gdpSource = scala.io.Source.fromFile("/users/mlewis/workspaceF18/CSCI3395-F18/data/BasicScala/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_10081022.csv")
    val gdpLines = gdpSource.getLines()
  }
}