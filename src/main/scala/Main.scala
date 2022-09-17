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

  //-2022-09-17- udkommenter foelgende linjer (dem der er relevante) hvis jeg skal finde nye elementer
  //val get10: List[(String, List[String])] = dataSearch.getPrintableTupples("lz", 600, files)
  //println(get10)
  //println(files.idsLinesMap("芔"))
  //println(files.zhengmaMap("𩬍"))
  //println(files.zmMap("平"))

  println("end of program")