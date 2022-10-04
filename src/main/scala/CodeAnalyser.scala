import caseClasses.{CharContent, CustomTree, LeafInput}

class CodeAnalyser {


  def mismatchingCodes(fileReader: InputSystemFileReader): List[(String, String, List[String], CharContent)] =
    val newCodeToContent: List[(String, List[String], CharContent)] = getCode(fileReader)
    val mismatchRemoved: List[(String, List[String], CharContent)] = newCodeToContent
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

    val diagnoseClash = diagnose(onlyClash)


    //getAllCodesWith 4 or more elements
    //val only4ormore = codes.filter(each => each._2.split(" ").size > 3)
    //val fourPlusCodeDisam: List[(String, String, String, CharContent)] = only4ormore.map(each => fourOrMoreDisamCodeList(each))

    //2022-10-02: jeg 20 der ikke matcher. jeg maa proeve flere loesninger
    //val newGroupBy = fourPlusCodeDisam.groupBy(c => c._3).values.toList.filter(each => each.size > 1).toList

    //val allDiffFullCodeLengths = onlyClash.filter(each => twoOrMoreSameElems(each)) //129
    //59 tilbagevaerende konflikter er for mange. proev at
    //kigge de 129 igennem og se efter stroeg. maaske sidste stroeg fra 1. og 2. element.
    //val allDiffStartEndInitials = allDiffFullCodeLengths.filter(each => compareFandSinit(each)) //59
    return List()

  private def diagnose(value: List[(String, List[String], CharContent)]): List[(String, List[String], CharContent)] =
    val fourOrMore = value.filter(each => each._2.size > 3)
    val firstGroup = fourOrMore.groupBy(c => c._1).values.toList.filter(each => each.size > 1).toList
    val leftRight = fourOrMore.filter(each => each._3.originalShapeLookup.charAt(0).equals('⿰'))
    val secondGroup = leftRight.groupBy(c => c._1).values.toList.filter(each => each.size > 1).toList
    val firstElemOneOrTwo = leftRight.filter(each => each._2(0).size > 4)
    val thirdGroup = firstElemOneOrTwo.groupBy(c => c._1).values.toList.filter(each => each.size > 1).toList
    val secElemOneOrTwo = firstElemOneOrTwo.filter(each => each._2(1).size > 4)
    val forthGroup = secElemOneOrTwo.groupBy(c => c._1).values.toList.filter(each => each.size > 1).toList

    val testDianosis: List[(String, String, List[String], CharContent)] = fourOrMore.map(each => testDiag(each))
    val fifthGroup = testDianosis.groupBy(c => c._2).values.toList.filter(each => each.size > 1).toList
    fourOrMore

  private def testDiag(value: (String, List[String], CharContent)): (String, String, List[String], CharContent) =
    val leftRight: String = if value._3.originalShapeLookup.charAt(0).equals('⿰') then "1" else "0"
    //val firstElemOneOrTwo: String = if value._2(1).size > 4 then "1" else "0"
    val secElemOneOrTwo: String = if value._2(2).size > 4 then "1" else "0"
    val lastStrokeOfLast: String = value._2.last.charAt(2).toString
    return (value._1, value._1 ++ leftRight ++ secElemOneOrTwo ++ lastStrokeOfLast, value._2, value._3)
  private def fourOrMoreDisamCodeList(tuple: (String, String, CharContent)): (String, String, String, CharContent) =
    val fullCode: List[String] = tuple._2.split(" ").map(each => each.trim).filter(each => each.nonEmpty).toList
    val disamCodeString: String = fourOrMoreDisamCode(fullCode, tuple._3)
    return (tuple._1, tuple._2, tuple._1 ++ disamCodeString, tuple._3)

  private def fourOrMoreDisamCode(strings: List[String], content: CharContent): String =
    var noInit: List[String] = strings.map(each => each.substring(3)) //each.substring(3)
    var onlyInit: List[String] = strings.map(each => each.substring(0, 3))
    if (noInit == "j b nd o".split(" ").toList) {
      val test = ""
    }
    if (strings.length < 4) {println("fourOrMoreDisamCode error: " ++ strings.mkString);return ""}
    val pri_2_index: Int = 1
    val pri_3_index: Int = 2
    (strings, noInit.last.length, noInit(pri_2_index).length, noInit(pri_3_index).length) match
      case (_, 1, 1, 1) => getStrokeType('z', strings, content)//"z"
      case (_, 1, 1, _) => getStrokeType(noInit(pri_3_index).last, strings, content)
      case (_, 1, _, 1) => getStrokeType(noInit(pri_2_index).last, strings, content)
      case (_, 1, _, _) => getStrokeType(noInit(pri_2_index).last, strings, content)
      //case (_, _, 1, 1) => getStrokeType(noInit.last.last, noInit(pri_2_index)(0))
      //case (_, _, 1, _) => getStrokeType(noInit.last.last, noInit(pri_3_index).last)
      //case (_, _, _, 1) => getStrokeType(noInit.last.last, noInit(pri_2_index).last)
      case (_, _, _, _) => getStrokeType(noInit.last.last, strings, content)

  private def getStrokeType(first: Char, second: List[String], content: CharContent): String =
    val firstHit: Int = getStrokeTypeHelper(first, content)
    val secondHit: Int = getPrimSecFromFirstSecThirdAndForth(second, content)
    return firstHit.toString ++ secondHit.toString

  private def getPrimSecFromFirstSecThirdAndForth(value: List[String], content: CharContent): Int =
    val thirdLast: Char = value(2).last
    (true, thirdLast) match
      case (_, 'a') => 0
      case (_, _) => 1
      //case (_, 'a') => 2
      //case _ => 4
  //var fir: Char = value(2).charAt(3)
  //val las: Char = value.last.charAt(1)
  //val secLast: Int = value(1).size
  //return 0
  //val secondHit: Option[Int] = if (second.isDefined) then getPrimsec(second)//8 == prim, 9 ==sec//
      // then getStrokeTypeHelper(second.get) else None
  /*  //when we have the strokeTypes, we need to translate it into an alphabet key
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
  */


  private def getStrokeTypeHelper(codeLetter: Char, content: CharContent): Int =
    val smallLetter: Option[Int] = getCodeLetterType(codeLetter, content)
    return smallLetter.get
    //val primarySecondary: Option[Int] =

  //return an integer from 0 to 4, matching the index of the alphabet
  private def getCodeLetterType(codeLetter: Char, content: CharContent): Option[Int] =
    val firstStrokes = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    val secondStrokes = List('i', 'j', 'k', 'l')
    val thirdStroeks = List('m', 'n', 'o', 'p', 'q', 'r')
    val forthStroeks = List('s', 't', 'u', 'v', 'w')
    val fifthStroeks = List('x', 'y', 'z')
    var lastStroke: Int = -1
    if firstStrokes.contains(codeLetter) then lastStroke = 0//Some('a')
    if secondStrokes.contains(codeLetter) || fifthStroeks.contains(codeLetter) then lastStroke = 1//Some('i')
    //if thirdStroeks.contains(codeLetter) then return Some(2)//Some('m')
    if forthStroeks.contains(codeLetter) || thirdStroeks.contains(codeLetter) then lastStroke = 2 //Some('s')
    //if fifthStroeks.contains(codeLetter)  then return Some(4) else return None//Some('x') else return None
    val sideElem: Boolean = content.originalShapeLookup.charAt(0).equals('⿰')
    if (lastStroke == -1) {
      val test = "test"
    }
    if (sideElem) then return Some(lastStroke + 3) else return Some(lastStroke)


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

  def getCode(fileReader: InputSystemFileReader): List[(String, List[String], CharContent)] =
    val treeGen: CharTreeGenerator = new CharTreeGenerator()
    val content: List[CharContent] = treeGen.createElemTreesFromChars(fileReader, 5000, -1)
    //val contentList
    val codeTupple: List[(String, List[String], CharContent)] = content
      .map(each =>
        (getCodeFromContent(each),
          getLongCodeFromContent(each), each))
    codeTupple

  private def getLongCodeFromContent(content: CharContent): List[String] =
    val inputLeafs: List[LeafInput] = content.flattenedTree.map(each => treeIsInputLeaf(each)).filter(x => x != null)
    val listOfInputs: String = inputLeafs.map(each => each.inputCode).mkString(" ")
    val res: List[String] = listOfInputs.split(" ").map(x => x.trim).filter(y => y != "").toList
    return res
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
