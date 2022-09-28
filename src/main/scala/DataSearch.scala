import caseClasses.{CharContent, CharacterBreakdown, CustomTree}

//fileReader: InputSystemFileReader
class DataSearch {
  val breakdownUtil: BreakdownUtilities = new BreakdownUtilities

  //val result: CharacterBreakdown = charResult(currentTzai, files)
  def getBreakdowns(strToGet: String, numOfLines: Int, fileReader: InputSystemFileReader): List[CharacterBreakdown] =
    val lines = getZhengmaLines(strToGet, numOfLines, fileReader)
    val res: List[CharacterBreakdown] = lines.map(each => breakdownUtil.charResult(each(0), fileReader))
    res

  def getElemsWithoutTDorLRshapemarker(fileReader: InputSystemFileReader): List[CharContent] =
    val treeGen: CharTreeGenerator = new CharTreeGenerator
    val treeFromChar: List[CharContent] = treeGen.createElemTreesFromChars(fileReader, 15000, -1)
    val shapes: List[String] = fileReader.shapeCharsMap.keys.toList
    val treesWithNoShapes: List[CharContent] = treeFromChar.filter(!hasNeededShape(shapes, _))
    return treesWithNoShapes

    //CharContent(override val elemStr: String,
  //                       freqInfo: List[String],
  //                       officialInputCodes: List[String],
  //                       binaryTree: CustomTree,
  //                       flattenedTree: List[CustomTree],
  //                       problemElems: List[CustomTree]) extends CustomTree

  private def hasNeededShape(shapes: List[String], content: CharContent): Boolean =
    val flattenedTreeFirst: CustomTree = content.flattenedTree(0)
    val firstElemOfStr: String = flattenedTreeFirst.elemStr.take(1)
    val origShapeOpt: String = content.originalShapeLookup
    val finalOrig: String = if (origShapeOpt.length > 0) then origShapeOpt.take(1) else ""
    val shapeInOrigShapeLokup: Boolean = shapes.contains(finalOrig)
    val shapeInFlattenedTree: Boolean = shapes.contains(firstElemOfStr)
    if (shapeInOrigShapeLokup) then true
    else if(shapeInFlattenedTree) then true
    else false


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
