import caseClasses.{CharContent, CustomTree, LeafInput}

class CodeAnalyser {


  def mismatchingCodes(fileReader: InputSystemFileReader): List[(String, String, List[String], CharContent)] =
    val newCodeToContent: List[(String, String, CharContent)] = getCode(fileReader)
    val mismatchRemoved: List[(String, String, CharContent)] = newCodeToContent
      .filter(each => !newCodeMatches(each._1, each._3.officialInputCodes))
    val displayInfo: List[(String, String, List[String], CharContent)] =
      mismatchRemoved.map(each => (each._1, each._3.freqInfo(3), each._3.officialInputCodes, each._3))
    return displayInfo

  private def newCodeMatches(newCode: String, officialCodes: List[String]): Boolean =
    val simpelMatch: Boolean = officialCodes.contains(newCode)
    (newCode.size, simpelMatch) match
      case (_, true) => true
      case (x, _) if x < 3 && endInVVmatch(newCode, officialCodes) => endInVVmatch(newCode, officialCodes)
      case _ if endInAsMatch(newCode, officialCodes) => endInAsMatch(newCode, officialCodes)
      case _ => false

  private def endInAsMatch(str: String, officialCodes: List[String]): Boolean =
    val singlea = officialCodes.contains(str ++ "a")
    val doublea = officialCodes.contains(str ++ "aa")
    val tripplea = officialCodes.contains(str ++ "aaa")
    val res: Boolean = singlea || doublea || tripplea
    return res

  private def endInVVmatch(str: String, officialCodes: List[String]): Boolean =
    val matchIfVVadded = officialCodes.contains(str ++ "vv")
    if matchIfVVadded then true else false

  def getInconsistencies(fileReader: InputSystemFileReader): List[List[(String, String, CharContent)]] =
    val codes = getCode(fileReader)
    val grupByCodes = codes.groupBy(c => c._1).values.toList
    val onlyClash = grupByCodes.filter(each => each.size > 1).toList.flatten //273

    //getAllCodesWith 4 or more elements
    val only4ormore = codes.filter(each => each._2.split(" ").size > 3)
    val fourPlusCodeDisam: List[(String, String, String, CharContent)] = only4ormore.map(each => fourOrMoreDisamCodeList(each))

    //2022-10-02: jeg 20 der ikke matcher. jeg maa proeve flere loesninger
    val newGroupBy = fourPlusCodeDisam.groupBy(c => c._3).values.toList.filter(each => each.size > 1).toList

    //val allDiffFullCodeLengths = onlyClash.filter(each => twoOrMoreSameElems(each)) //129
    //59 tilbagevaerende konflikter er for mange. proev at
    //kigge de 129 igennem og se efter stroeg. maaske sidste stroeg fra 1. og 2. element.
    //val allDiffStartEndInitials = allDiffFullCodeLengths.filter(each => compareFandSinit(each)) //59
    return List()

  private def fourOrMoreDisamCodeList(tuple: (String, String, CharContent)): (String, String, String, CharContent) =
    val fullCode = tuple._2.split(" ").map(each => each.trim).filter(each => each.nonEmpty)
    var noInit: List[String] = null
    try {
      noInit = fullCode.map(each => each.substring(3)).toList
    }catch {
      case e => println("error: " ++ tuple._1 ++ tuple._2 ++ tuple._3.elemStr)
      val test = ""
    }
    val disamCodeString: String = fourOrMoreDisamCode(noInit)
    return (tuple._1, tuple._2, tuple._1 ++ disamCodeString, tuple._3)

  private def fourOrMoreDisamCode(strings: List[String]): String =
    if (strings == "j b nd o".split(" ").toList) {
      val test = ""
    }
    if (strings.length < 4) {println("fourOrMoreDisamCode error: " ++ strings.mkString);return ""}
    val pri_2_index: Int = 1
    val pri_3_index: Int = 2
    (strings, strings.last.length, strings(pri_2_index).length, strings(pri_3_index).length) match
      case (_, 1, 1, 1) => "z"
      case (_, 1, 1, _) => getStrokeType(Some(strings(pri_3_index).last), None)
      case (_, 1, _, 1) => getStrokeType(Some(strings(pri_2_index).last), None)
      case (_, 1, _, _) => getStrokeType(Some(strings(pri_2_index).last), Some(strings(pri_3_index).last))
      case (_, _, 1, 1) => getStrokeType(Some(strings.last.last), None)
      case (_, _, 1, _) => getStrokeType(Some(strings.last.last), Some(strings(pri_3_index).last))
      case (_, _, _, 1) => getStrokeType(Some(strings.last.last), Some(strings(pri_2_index).last))
      case (_, _, _, _) => getStrokeType(Some(strings.last.last), Some(strings(pri_2_index).last))

  private def getStrokeType(first: Option[Char], second: Option[Char]): String =
    val firstHit: Option[Int] = if first.isDefined then getStrokeTypeHelper(first.get) else None
    val secondHit: Option[Int] = if (second.isDefined) then getStrokeTypeHelper(second.get) else None
    //when we have the strokeTypes, we need to translate it into an alphabet key
    val alpha = ('a' to 'z').toList
    var result = ""
    var index: Int = 0
    if (secondHit.isEmpty) {index = firstHit.get * 5}
    else index = firstHit.get * 5 + secondHit.get
    val firstString: String = if firstHit.isDefined then firstHit.get.toString else "0"
    val secondString: String = if secondHit.isDefined then secondHit.get.toString else "0"
    return firstString ++ secondString
    //val finalResult: Char = alpha(index)
    //return finalResult.toString

  //return an integer from 0 to 4, matching the index of the alphabet
  private def getStrokeTypeHelper(codeLetter: Char): Option[Int] =
    val firstStrokes = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    val secondStrokes = List('i', 'j', 'k', 'l')
    val thirdStroeks = List('m', 'n', 'o', 'p', 'q', 'r')
    val forthStroeks = List('s', 't', 'u', 'v', 'w')
    val fifthStroeks = List('x', 'y', 'z')
    if firstStrokes.contains(codeLetter) then return Some(0)//Some('a')
    if secondStrokes.contains(codeLetter) then return Some(1)//Some('i')
    if thirdStroeks.contains(codeLetter) then return Some(2)//Some('m')
    if forthStroeks.contains(codeLetter) then return Some(3)//Some('s')
    if fifthStroeks.contains(codeLetter) then return Some(4) else return None//Some('x') else return None


  private def compareFandSinit(value: List[(String, String, CharContent)]): Boolean =
    val newInp: List[List[String]] = value.map(each => each._2.split(" ").toList)
    val firstAndSecInitials: List[String] = newInp.map(each => getFirstAndSecInit(each))
    return firstAndSecInitials.toSet.size > 1

  private def getFirstAndSecInit(i: List[String]): String =
    i match
      //case Nil => throw "getFirstAndSecInit empty code list"
      case x :: Nil => x.substring(0, 2)
      case _ => i(0).substring(0,2) ++ i.last.substring(0,2)
  private def twoOrMoreSameElems(input: List[(String, String, CharContent)]): Boolean =
    val newInp = input.map(each => each._2.split(" ").size)
    return newInp.toSet.size != newInp.size

  def getCode(fileReader: InputSystemFileReader): List[(String, String, CharContent)] =
    val treeGen: CharTreeGenerator = new CharTreeGenerator()
    val content: List[CharContent] = treeGen.createElemTreesFromChars(fileReader, 6000, -1)
    //val contentList
    val codeTupple: List[(String, String, CharContent)] = content
      .map(each => (getCodeFromContent(each), getLongCodeFromContent(each), each))
    codeTupple

  private def getLongCodeFromContent(content: CharContent): String =
    val inputLeafs: List[LeafInput] = content.flattenedTree.map(each => treeIsInputLeaf(each)).filter(x => x != null)
    val listOfInputs: String = inputLeafs.map(each => each.inputCode).mkString(" ")
    return listOfInputs
  private def getCodeFromContent(content: CharContent): String =
    val inputLeafs: List[LeafInput] = content.flattenedTree.map(each => treeIsInputLeaf(each)).filter(x => x != null)
    val listOfInputs: String = inputLeafs.map(each => each.inputCode).mkString(" ")
    val splitIT: List[String] =
      listOfInputs.split(" ")
        .map(each => each.trim)
        .filter(x => !(x.length == 0)).toList
    //test hvis splitItHarStrings med laengde under 3
    val lenLessThan4: Boolean = splitIT.filter(x => x.length < 4).toList.size > 0
    if (lenLessThan4) {
      val str: String = ""
    }
    val toOrdinaryCode: String = strListToCustomCode(splitIT)
    return toOrdinaryCode

  private def strListToCustomCode(input: List[String]): String =
    val withoutInitials = input.map(each => each.substring(3))
    val newCode = strListToCustomCodeHelper(withoutInitials, input)
    return newCode.mkString

  private def strListToCustomCodeHelper(withoutInitials: List[String], withIni: List[String]): List[String] =
    withoutInitials.size match
      case 1 => withoutInitials
      case 2 => fullTwoCode(withoutInitials)
      case 3 => fullThreeCode(withoutInitials, withIni)
      case _ => fullFourOrGreater(withoutInitials, withIni)

  private def fullFourOrGreater(value: List[String], withIni: List[String]): List[String] =
    val secondToLast = value.takeRight(2)(0)
    val last = value.last
    value(0).size match
      case 1 => List(value(0), value(1).take(1), secondToLast.take(1), last.take(1))
      //case 2 if withIni(0).last == 'd' && (withIni(0).substring(0,1) == "2")
      //  => List(value(0).take(1), value(1).take(1), secondToLast.take(1), last.take(1))
      //case _ if value(0).last == 'd' => List(value(0).take(1), value(1).take(1), secondToLast.take(1), last.take(1))
      case _ => List(value(0).take(2), secondToLast.take(1), last.take(1))

  private def fullThreeCode(arg: List[String], withIni: List[String]): List[String] =
    arg(0).size match
      case 1 => List(arg(0), arg(1).take(1), arg(2).take(2))
      //case 2 if withIni(0).last == 'd' && withIni(0).substring(0,1) == "2"
      //  => List(arg(0).take(1), arg(1).take(1), arg(2).take(2))
      case _ => List(arg(0).take(2), arg(1).take(1), arg(2).take(1))

  private def fullTwoCode(arg: List[String]): List[String] =
    arg(0).size match
      case 1 => arg
      case 2 => List(arg(0), arg(1).take(2))
      case 3 => List(arg(0), arg(1).take(1))

  private def treeIsInputLeaf(tree: CustomTree): LeafInput =
    tree match
      case LeafInput(elemStr, inputCode) => LeafInput(elemStr, inputCode)
      case _ => null

}
