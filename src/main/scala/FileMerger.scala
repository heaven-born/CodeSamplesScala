import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

import scala.io.Source

/**
  * Problem: Time Series Merge
  *
  * Time series are stored in files with the following format:
  *
  * ● files are multiline plain text files in ASCII encoding
  * ● each line contains exactly one record
  * ● each record contains date and integer value; records are encoded like so: YYYY-MM- DD:X
  * ● dates within single file are non-duplicate and sorted in ascending order
  * ● files can be bigger than RAM available on target host
  *
  * Script merges arbitrary number of files, up to 100, into one file. Result file follows same format
  * conventions as described above. Records with same date value is merged into one by summing up X values.
  */

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
    val fileStreamsSorted = files
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

    val streamWithAllRowsSorted = Stream.iterate(fileStreamsSorted){
      case (_ #:: Stream.Empty) :: tail =>  moveFirstElem(tail)
      case (_ #:: rest) :: tail => moveFirstElem(rest :: tail)
    }.takeWhile(_.nonEmpty).map(_.head.head)


    def mergeAndWrite(s: Stream[Row]):Unit =   {
      val (l,r) = s.span(_.date == s.head.date)
      val res = l.reduce((a,b) => Row(a.date, a.value+b.value))
      outFile.println(formatter.format(res.date)+ s":${res.value}")
      if (r.nonEmpty) mergeAndWrite(r)
    }

    mergeAndWrite(streamWithAllRowsSorted)

    //close sources
    files.foreach(_.close())
    outFile.close()

  }




}
