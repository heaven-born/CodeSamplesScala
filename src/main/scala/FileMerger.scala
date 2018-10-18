import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

import scala.io.Source

class FileMerger {

  private val DATE_FORMAT = "yyyy-MM-dd"
  private val dateFormat = new SimpleDateFormat(DATE_FORMAT)
  private val formatter = DateTimeFormatter.ofPattern(DATE_FORMAT).withZone( ZoneId.systemDefault() )

  implicit def strToDate(s: String) = dateFormat.parse(s)

  case class Row(date: Instant, value: Int)

  def merge(inputFiles: List[String], outputFile: String): Unit = {

    val files = inputFiles.map(Source.fromFile)
    val outFile = new PrintWriter(new File(outputFile))

    //NOTE: it is supposed that each file is valid and contains at least one line
    val filesWithFirstRow = files
      .map(_.getLines()
            .map(_.split(":"))
            .map(k=>Row(k(0).toInstant,k(1).toInt)))
      .map(k=>k.toStream)
      .sortBy(_.head.date)


    def moveFirstElem(l: List[Stream[Row]]) = l match {
      case Nil => Nil
      case (row #:: _) :: tail => {
        val (s1, s2) = tail.span(_.head.date isBefore row.date)
        s1 ::: l.head :: s2
      }
    }

    val streamWithAllRowsSorted = Stream.iterate(filesWithFirstRow){
      case (_ #:: Stream.Empty) :: tail =>  moveFirstElem(tail)
      case (_ #:: rest) :: tail => moveFirstElem(rest :: tail)
    }.takeWhile(_.nonEmpty).map(_.head.head)


    def span(s: Stream[Row]):Unit =   {
      val (l,r) = s.span(_.date == s.head.date)
      val res = l.reduce((a,b)=>Row(a.date, a.value+b.value))
      outFile.println(formatter.format(res.date)+ s":${res.value}")
      if (r.nonEmpty) span(r)
    }

    span(streamWithAllRowsSorted)

    //close sources
    files.foreach(_.close())
    outFile.close()

  }




}
