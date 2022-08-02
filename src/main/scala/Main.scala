import java.io.FileReader

@main def hello() =
  println("Hello, world")


  //println(files.idsLinesMap("中"))
  //println(files.idsLinesMap("缻"))

  val files: InputSystemFileReader = new InputSystemFileReader()
  files.loadIds()

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
  def searchFromChar(stringToSearch: String, originalString: String, fileReader: InputSystemFileReader): String = {
    var inputSystemMatch: String = if (fileReader.zmMap.contains(stringToSearch)) fileReader.zmMap(stringToSearch) else ""
    var idsMatch: String = if (fileReader.idsLinesMap.contains(stringToSearch)) fileReader.idsLinesMap(stringToSearch) else ""
    var shortestPossibleString: String = shortestStringWithMatch(stringToSearch, fileReader)

    if(!inputSystemMatch.isBlank) {
      return stringToSearch + " " + inputSystemMatch
    }else if (!idsMatch.isBlank && !stringToSearch.eq(idsMatch)) {
      return searchFromChar(idsMatch, originalString, fileReader)
    }else if (!idsMatch.isBlank){
      return idsMatch+"!!!!"
    }else if (shortestPossibleString.isBlank && stringToSearch.length == 1) {
      return stringToSearch+"!!!!"
    }else if (shortestPossibleString.isBlank) {
      return stringToSearch.substring(0,1) + " " +
        searchFromChar(stringToSearch.substring(1, stringToSearch.length), originalString, fileReader)
    }else {
      val part1: String = searchFromChar(stringToSearch.substring(0, shortestPossibleString.length), originalString,  fileReader)
      val part2: String = searchFromChar(stringToSearch.substring(shortestPossibleString.length, stringToSearch.length), originalString, fileReader)
      return part1 + " " + part2
    }

    /*
    if (!firstResult.isBlank) {
      return stringToSearch + " " + firstResult
    }else if (!secondResult.isBlank && stringToSearch != secondResult) {
      return stringToSearch + " " + stringFromChar(secondResult, secondResult, fileReader)
    }else if (!secondResult.isBlank) {
      return secondResult
    }else{
      if (stringToSearch.length == 1 && originalString.length > 1) {
        return stringToSearch + stringFromChar(originalString.substring(1), originalString.substring(1), fileReader)
      }else if (stringToSearch.length == 1){
        return stringToSearch
      } else {
        return stringFromChar(stringToSearch.substring(0, stringToSearch.length-1), originalString, fileReader)
      }
    }
    */
  }

  def charResult(stringToSearch: String, fileReader: InputSystemFileReader): String ={
    val firstIds: String = if (fileReader.idsLinesMap.contains(stringToSearch)) fileReader.idsLinesMap(stringToSearch) else ""
    if (firstIds.isBlank) {
      return "!!!!"
    }else {
      return stringToSearch +
        " " +
        firstIds.substring(0,1) +
      " " +
      firstIds  +
        " " +
        searchFromChar(firstIds.substring(1, firstIds.length), stringToSearch, fileReader)
    }
  }

  //val result = searchFromChar("缻", "缻", files)

  println(charResult("的", files))
  println(charResult("是", files))


  //println(files.idsLinesMap("缶"))
  //println(files.idsLinesMap("瓦"))


  //println(files.idsRawLines.size)
  //println(files.idsLinesMap("缻"))
  //println(files.zmMap("夂"))




  println("end of program")