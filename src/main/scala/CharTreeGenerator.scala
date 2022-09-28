import caseClasses.{CharContent, CustomTree, LeafIds, LeafInput, LeafNone,
  LeafShape, LeafStroke, NodeAny}

class CharTreeGenerator {

  def createElemTreesFromChars(fileReader: InputSystemFileReader,
                               charsToTake: Int,
                               getOnlyFromIndex: Int): List[CharContent] =
    var firstChars: List[List[String]] = null
    if (getOnlyFromIndex < 0) {
      firstChars = fileReader.tzaiLines.take(charsToTake)
    }else {
      firstChars = List(fileReader.tzaiLines(getOnlyFromIndex))
    }
    val result: List[CharContent] = firstChars.map(eachList => charToContent(eachList, fileReader))
    return result

  def charToContent(systemLines: List[String], fileReader: InputSystemFileReader): CharContent =
    val binaryTree: CustomTree = charToTree(systemLines(0), fileReader)
    val flattenRaw: List[CustomTree] = flattenBinary(binaryTree)
    val flattenTree: List[CustomTree] = flattenRaw.filter(each => !each.isInstanceOf[LeafNone])
    val issueElems: List[CustomTree] = flattenRaw
      .filter(each => (each.isInstanceOf[LeafNone] || each.isInstanceOf[LeafStroke]) && each.elemStr != "")
    val zhengLookup: List[String] =
      if fileReader.zhengmaMap.get(systemLines(0)).isEmpty then List()
      else fileReader.zhengmaMap.get(systemLines(0)).get
    val originalLookupOpt: Option[String] = fileReader.idsLinesMap.get(systemLines(0))
    val origLookup: String = if (originalLookupOpt.isDefined) originalLookupOpt.get else ""
    CharContent(systemLines(0), systemLines, zhengLookup, binaryTree, flattenTree, issueElems, origLookup)

  def flattenBinary(input: CustomTree): List[CustomTree] =
    input match
      case nodeany: NodeAny =>
        val firstList: List[CustomTree] = flattenBinary(nodeany.head)
        val endList: List[CustomTree] = flattenBinary(nodeany.tail)
        return firstList ++ endList
      case empty: LeafNone => List(empty)
      case other: CustomTree => List(other)

  def charToTree(charStr: String, fileReader: InputSystemFileReader): CustomTree =
    val inputMatch: (Option[String], String, Int) = elemsToTargetAndMatchingStr(charStr, fileReader.zmMap)
    val idsMatch: (Option[String], String, Int) = elemsToTargetAndMatchingStr(charStr, fileReader.idsLinesMap)
    val idsMatchesInput: Boolean = if idsMatch._1.isDefined && idsMatch._1.get == charStr then true else false
    val strokeMatch: (Option[String], String, Int) = elemsToTargetAndMatchingStr(charStr, fileReader.strokesMap)
    val shapeMatch: (Option[String], String, Int) = elemsToTargetAndMatchingStr(charStr, fileReader.shapeCharsMap)
    val matches = (inputMatch._1, idsMatch._1, strokeMatch._1, shapeMatch._1, idsMatchesInput)
    matches match
      //no hit is found on either input or ids
      case (None, None, None, None, _) => LeafNone(charStr)
      //an input method hit is found
      case (Some(inpm), _, _, _, _) => NodeAny(charStr, LeafInput(inputMatch._2, inputMatch._1.get), charToTree(charStr.substring(inputMatch._3), fileReader))
        //NodeAny(charStr, charToTree(inputMatch._2, fileReader), charToTree(charStr.substring(inputMatch._3), fileReader))
       //an ids match is found that matches the input string
      case (_, _, _, _, true) => LeafNone(charStr)
      //an ids match is found that doesnt match the input string
      case (_, Some(idsm), _, _, _) => createNode(charStr, idsm, idsMatch._3, fileReader)
        //NodeAny(charStr, charToTree(idsMatch._2, fileReader), charToTree(charStr.substring(idsMatch._3), fileReader))
      //a stroke is found
      case (_, _, Some(stroke), _, _) => NodeAny(charStr, LeafStroke(strokeMatch._2, strokeMatch._1.get), charToTree(charStr.substring(strokeMatch._3), fileReader))
        //NodeAny(charStr, charToTree(strokeMatch._2, fileReader), charToTree(charStr.substring(strokeMatch._3), fileReader))
      //a shape is found
      case (_, _, _, Some(shape), _) => NodeAny(charStr, LeafShape(shapeMatch._2, shapeMatch._1.get), charToTree(charStr.substring(shapeMatch._3), fileReader))

  def createNode(origStr: String, foundSubStr: String, lengthOfSub: Int, fileReader: InputSystemFileReader): CustomTree =
    NodeAny(origStr, charToTree(foundSubStr, fileReader), charToTree(origStr.substring(lengthOfSub), fileReader))


  def elemsToTargetAndMatchingStr(charOrElems: String, map: Map[String, String]): (Option[String], String, Int) =
    val elemMatch: Option[String] = map.get(charOrElems)
    val matcherObj: (Option[String], String, Int) = (elemMatch, charOrElems, charOrElems.size)
    matcherObj match
      case (_, _, 0) => (None, "", 0)
      case (None, _, _) => elemsToTargetAndMatchingStr(charOrElems.dropRight(1), map)
      case _ => (matcherObj._1, matcherObj._2, matcherObj._3)


}
