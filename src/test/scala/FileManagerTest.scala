
import org.scalatest._
import scala.io.Source

class FileManagerTest  extends FlatSpec with Matchers {

  "A FileMerger" should "merge files" in {

    val inFiles = "src/test/resources/file2.txt" ::
                  "src/test/resources/file3.txt" ::
                  "src/test/resources/file1.txt" :: Nil

    val out = "src/test/resources/out.txt"

    val fm = new FileMerger()
    fm.merge(inFiles,out)

    val outList: Seq[String] = Source.fromFile(out).getLines().toList


    outList should be(
      Seq(
        "2017-01-12:11",
        "2018-01-11:111",
        "2018-01-12:122",
        "2018-01-13:113",
        "2018-01-17:10",
        "2018-02-14:77",
        "2018-02-18:18",
        "2018-02-19:18",
        "2018-03-01:1",
        "2018-04-20:16",
        "2018-04-21:88",
        "2018-04-22:115"
      )
    )



  }


}
