import caseClasses.{CharacterBreakdown, ComponentInfo}

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

class BreakdownUtilities {

  def shortestStringWithMatch(stringToSearch: String, fileReader: InputSystemFileReader): String = {
    var firstResult: String = if (fileReader.zmMap.contains(stringToSearch)) fileReader.zmMap(stringToSearch) else ""
    var secondResult: String = if (fileReader.idsLinesMap.contains(stringToSearch)) fileReader.idsLinesMap(stringToSearch) else ""
    if (firstResult.isEmpty && secondResult.isEmpty && stringToSearch.length < 2) {
      return ""
    }else if (!firstResult.isEmpty || !secondResult.isEmpty) {
      return stringToSearch
    }else {
      return shortestStringWithMatch(stringToSearch.substring(0, stringToSearch.length-1), fileReader)
    }
  }

  //write a function that takes a character and the files object and
  //returns a string of character + code + search findings
  def searchFromChar(stringToSearch: String,
                     originalString: String,
                     fileReader: InputSystemFileReader): String = {
    var inputSystemMatch: String = if
    (fileReader.zmMap.contains(stringToSearch))
      ("{"+stringToSearch + " " + fileReader.zmMap(stringToSearch)+"}") else ""
    if (fileReader.zmMap.contains(originalString)) inputSystemMatch =
      ("{"+ originalString + " " +fileReader.zmMap(originalString) +"}")
    var idsMatch: String = if (fileReader.idsLinesMap.contains(stringToSearch)) fileReader.idsLinesMap(stringToSearch) else ""
    var shortestPossibleString: String = shortestStringWithMatch(stringToSearch, fileReader)

    if(!inputSystemMatch.isEmpty) {
      return stringToSearch + " " + inputSystemMatch
    }else if (!idsMatch.isEmpty && !stringToSearch.eq(idsMatch)) {
      return searchFromChar(idsMatch, originalString, fileReader)
    }else if (!idsMatch.isEmpty){
      return idsMatch+"!!!!"
    }else if (shortestPossibleString.isEmpty && stringToSearch.length == 1) {
      return stringToSearch+"!!!!"
    }else if (shortestPossibleString.isEmpty) {

      //stringToSearch might be empty here
      if (stringToSearch.size == 0) {
        return ""
      }else {
        return stringToSearch.substring(0, 1) + " " +
          searchFromChar(stringToSearch.substring(1, stringToSearch.length), originalString, fileReader)
      }
    }else {
      val part1: String = searchFromChar(stringToSearch.substring(0, shortestPossibleString.length), originalString,  fileReader)
      val part2: String = searchFromChar(stringToSearch.substring(shortestPossibleString.length, stringToSearch.length), originalString, fileReader)
      return "{" + part1 + "}" + " " + "{" + part2 + "}"
    }
  }

  def charResult(stringToSearch: String, fileReader: InputSystemFileReader): CharacterBreakdown ={
    val firstIds: String = if (fileReader.idsLinesMap.contains(stringToSearch)) fileReader.idsLinesMap(stringToSearch) else ""
    var character: String = stringToSearch;
    var fullComponentString: String = null;
    var firstComponent: String = null;
    var componentList: ListBuffer[ComponentInfo] = ListBuffer();
    if (firstIds.isEmpty) {
      return CharacterBreakdown(character, fullComponentString, firstComponent, componentList.toList, "no character match")
    }else {
      fullComponentString = firstIds;
      firstComponent = firstIds.substring(0, 1);
      val rawResult: String = searchFromChar(firstIds.substring(1, firstIds.length), stringToSearch, fileReader)
      //test that there are no !!!! marks
      if ((rawResult contains "!!!!") || rawResult.size == 0) {
        return CharacterBreakdown(character, fullComponentString, firstComponent, componentList.toList, "subcomponentCantBeFound")
      }else {
        val pattern = new Regex("\\{[^{}]+\\}");
        val regexedResult: String = (pattern findAllIn rawResult).mkString("---");
        val componentInfoMaterial: List[String] = regexedResult.split("---").toList
        val componentInfoList: List[ComponentInfo] = componentInfoMaterial.map(each => createComponentFromSpaceString(each)).toList
        return CharacterBreakdown(character, fullComponentString, firstComponent, componentInfoList, "");
      }
    }
  }

  def goThroughTzaiList(numberOfChars: Int, files: InputSystemFileReader): ListBuffer[CharacterBreakdown] = {
    var breakdownList: ListBuffer[CharacterBreakdown] = ListBuffer()
    var endThis: String = ""
    var errorTzai: List[String] = null;
    var errorBreakdown: CharacterBreakdown = null;

    for (i <- List.range(0, numberOfChars)) {
      val currentTzai: String = files.tzaiLines(i)(0)
      val result: CharacterBreakdown = charResult(currentTzai, files)
      breakdownList += result
      if (result.errorMessage.size > 0 && errorTzai == null) {
        println(files.tzaiLines(i))
        println(result)
        errorTzai = files.tzaiLines(i)
        errorBreakdown = result
      }
    }
    return breakdownList;
  }

  def createComponentFromSpaceString(each: String): ComponentInfo = {
    val splittetString: List[String] = each.split(" ").toList;
    return ComponentInfo(splittetString(0), splittetString.drop(1).mkString(" "))
  }

  //udkommenter for at se et udprint af foerste karakter der ikke er med i min tegn map.
  //val result: ListBuffer[CharacterBreakdown] = goThroughTzaiList(20, files)

  //result.toList.foreach(each => println(each))


}
