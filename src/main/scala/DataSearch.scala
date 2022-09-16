import caseClasses.CharacterBreakdown

//fileReader: InputSystemFileReader
class DataSearch {
  val breakdownUtil: BreakdownUtilities = new BreakdownUtilities

  //val result: CharacterBreakdown = charResult(currentTzai, files)
  def getBreakdowns(strToGet: String, numOfLines: Int, fileReader: InputSystemFileReader): List[CharacterBreakdown] =
    val lines = getZhengmaLines(strToGet, numOfLines, fileReader)
    val res: List[CharacterBreakdown] = lines.map(each => breakdownUtil.charResult(each(0), fileReader))
    res

  def getPrintableTupples(strToGet: String, numOfLines: Int, fileReader: InputSystemFileReader): List[(String, List[String])] =
    val lines = getZhengmaLines(strToGet, numOfLines, fileReader)
    val linesToTupple: List[(String, List[String])] = lines.map(each => (each(0), each.drop(1)))
    linesToTupple

  //val breakdowns = lines.map(charResult(_(0), fileReader))

    //zhengmaMap: Map[String, List[String]]
    //val zhengmaLines

  private def getZhengmaLines(strToGet: String, numOfLines: Int, fileReader: InputSystemFileReader): List[List[String]] =
    val zhengLines: List[List[String]] =
      fileReader.zhengmaLines.filter(each => lineContainString(strToGet, each))
    return zhengLines.take(numOfLines)

  private def lineContainString(strToGet: String, zhengLine: List[String]): Boolean =
    val lineHas = zhengLine.drop(1).filter(_.contains(strToGet))
    lineHas.size > 0



}
