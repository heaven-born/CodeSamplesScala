
import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

import scala.io.Source
import scala.collection.immutable._

class FileMerger {

  private val DATE_FORMAT = "yyyy-MM-dd"
  private val dateFormat = new SimpleDateFormat(DATE_FORMAT)
  private val formatter = DateTimeFormatter.ofPattern(DATE_FORMAT).withZone(ZoneId.systemDefault())

  implicit def strToDate(s: String) = dateFormat.parse(s)

  case class Row(date: Instant, value: Int)

  def merge(inputFiles: List[String], outputFile: String): Unit = {

    val files = inputFiles.map(Source.fromFile)
    val outFile = new PrintWriter(new File(outputFile))

    //NOTE: it is supposed that each file is valid and contains at least one line
    val fileStreamsSorted = files
      .map(_.getLines()
            .map(_.split(":"))
            .map(k => Row(k(0).toInstant, k(1).toInt)))
      .map(_ to LazyList)

    def mergeLists(l1: LazyList[Row], l2:LazyList[Row]):LazyList[Row] = (l1,l2) match {
      case (l1,LazyList()) => l1
      case (LazyList(),l2) => l2
      case (h1 #:: t1, h2 #:: t2) =>
        if (h1.date isBefore h2.date)
          h1 #:: mergeLists(t1, l2)
        else
          h2 #:: mergeLists(l1, t2)
    }

    def groupList(ls: LazyList[Row]) :LazyList[Row]= ls match {
      case LazyList() => ls
      case h1 #:: LazyList() => ls
      case h1 #:: h2 #:: tail =>
        if (h1.date==h2.date)
           groupList(Row(h1.date,h1.value+h2.value) #:: tail)
        else
           h1 #:: groupList(h2 #:: tail)
    }


    import scala.util.chaining._

    fileStreamsSorted
      .reduce(mergeLists)
      .pipe(groupList)
      .foreach{ res =>
          outFile.println(formatter.format(res.date)+ s":${res.value}")
      }

    //close sources
    files.foreach(_.close())
    outFile.close()

  }


}