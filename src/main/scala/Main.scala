import caseClasses.{CharacterBreakdown, ComponentInfo}
import java.io.FileReader
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

@main def hello() =
  println("Hello, world")

  val files: InputSystemFileReader = new InputSystemFileReader()
  files.loadIds()
  files.loadTzai()
  files.loadZhengma()


  println("end")

  //println(files.idsLinesMap("瓦"))
  //println(files.idsRawLines.size)
  //println(files.idsLinesMap("缻"))
  //println(files.zmMap("夂"))

//skriv en funktion der finder tegn baseret paa zhengma alm koder.
  val dataSearch: DataSearch = new DataSearch
  val get10: List[(String, List[String])] = dataSearch.getPrintableTupples("u", 200, files)
  //println(get10)
  println(files.idsLinesMap("忝"))

  println("end of program")