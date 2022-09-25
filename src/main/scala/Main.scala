import caseClasses.{CharacterBreakdown, ComponentInfo, CustomTree}

import java.io.FileReader
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

@main def hello() =
  println("Hello, world")

  val files: InputSystemFileReader = new InputSystemFileReader()
  files.loadIds()
  files.loadTzai()
  files.loadZhengma()
  files.loadShapeChars()
  files.loadStrokes()


  println("end")

  //println(files.idsLinesMap("瓦"))
  //println(files.idsRawLines.size)
  //println(files.idsLinesMap("缻"))
  //println(files.zmMap("夂"))

//skriv en funktion der finder tegn baseret paa zhengma alm koder.
  val dataSearch: DataSearch = new DataSearch

  val treeGen: CharTreeGenerator = new CharTreeGenerator
  val treeFromChar = treeGen.createElemTreesFromChars(files, 15000, -1)
  val tupples: List[(String, List[String], CustomTree, List[CustomTree], List[CustomTree])] =
    treeFromChar.map(each => (each.elemStr, each.officialInputCodes, each.binaryTree, each.flattenedTree, each.problemElems))
  val problemTupples = tupples.filter(each => !each._5.isEmpty)
  val smallProblemTupp = problemTupples.take(3)
  //val test = files.idsLinesMap.get("以")

  println("end")
  //-2022-09-17- udkommenter foelgende linjer (dem der er relevante) hvis jeg skal finde nye elementer
  //val get10: List[(String, List[String])] = dataSearch.getPrintableTupples("lz", 600, files)
  //println(get10)
  println(files.idsLinesMap("巤"))
  //println(files.zmMap("幺")) //𩬍
  //println("曲" + files.zhengmaMap.get("曲")) // 豆  東  㠯
  //println("豆" + files.zhengmaMap.get("豆"))
  println("儰" + files.zhengmaMap.get("儰"))// 斷 㡭斤  𢆸𢆸  ⿺𠃊𢆶
  println("嬽" + files.zhengmaMap.get("嬽"))
  println("𡚇" + files.zhengmaMap.get("𡚇")) //⿱⿱亠⿲刀丫③⿲丿二丨
  //println(files.zhengmaMap.get("㠯"))
  println("end of program")