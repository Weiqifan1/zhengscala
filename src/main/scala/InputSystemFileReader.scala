import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.NonFatal

class InputSystemFileReader {

  var idsLinesMap: Map[String, String] = Map();
  var idsRawLines: List[List[String]] = null;
  var tzaiLines: List[List[String]] = null;
  var zhengmaLines: List[List[String]] = null;
  var zhengmaMap: Map[String, List[String]] = Map();
  var tzaiSimpelList: List[String] = null;
  var shapeCharsMap: Map[String, String] = Map()
  var strokesMap: Map[String, String] = Map()

  def getIdsLinesMap() : Map[String, String] = idsLinesMap
  def getIdsRawLines() : List[List[String]] = idsRawLines

  def getTzaiLines(): List[List[String]] = tzaiLines
  def getZhengmaMap(): Map[String,List[String]] = zhengmaMap
  
  def loadZhengma() = {
    var tempLines = new ListBuffer[List[String]]()
    val source = Source.fromFile("./src/resources/zhengmaChar.txt")

    for (eachLineElem: String <- source.getLines()) {
      if (eachLineElem != null && eachLineElem.size > 1) {
        tempLines += eachLineElem.split("\\s+").toList
      }
    }

    //create a list of 2 items, char and all matches
    //val

    val joinedLines: Map[String, List[List[String]]] = tempLines.toList.groupBy(each => each(0))
    //val joinedLines: Map[String, List[String]] = tempLines.toList.groupBy(removeCharandFlatten(each) => each(0))



    zhengmaMap = joinedLines.map(tup => flattenPair(tup._1, tup._2))

    def flattenPair(key: String, eachTup: List[List[String]]): (String, List[String]) = {
      val tempList: List[String] = eachTup.flatten.filter(eachElem => !eachElem.equals(key)).toSet.toList
      return Tuple2(key, tempList)
    }

    zhengmaLines = tempLines.toList
/*
    for (eachSubList <- zhengmaLines) {
      if (eachSubList != null
        && eachSubList.size > 2
        && eachSubList(1) != null
        && eachSubList(1).size > 0
        && eachSubList(2) != null
        && eachSubList(2).size > 0) {
        zhengmaMap = zhengmaMap + (eachSubList(0) -> eachSubList)
      }
    }
    */
    source.close()
  }

  def loadTzai() = {
    var tempLines = new ListBuffer[List[String]]() //List[List[String]] = List()
    //var tempHash: Map[String, String] = Map();
    val source = Source.fromFile("./src/resources/tzai.txt")
    val withIndex = source.getLines().zipWithIndex
    for (eachLineElem <- withIndex) {
      if (eachLineElem._1 != null && eachLineElem._1.size > 1) {
        tempLines += eachLineElem._1.split("\\s+").toList.appended((eachLineElem._2 + 1).toString)
      }
    }
    //tempLines.toList.foreach(x => idsLinesMap + (x(1) -> x(2)))
    tzaiLines = tempLines.toList
    source.close()
    
    //tzaiSimpelList
    tzaiSimpelList = tempLines.toList.map(each => each(0))
  }

  def loadShapeChars() = {
    shapeCharsMap = HashMap(
        "⿰" -> "⿰",
        "⿱" -> "⿱",
        "⿲" -> "⿲",
        "⿳" -> "⿳",
        "⿴" -> "⿴",
        "⿵" -> "⿵",
        "⿶" -> "⿶",
        "⿷" -> "⿷",
        "⿸" -> "⿸",
        "⿹" -> "⿹",
        "⿺" -> "⿺",
        "⿻" -> "⿻"
    )
  }


  def loadStrokes() = {
    strokesMap = HashMap(
      "㇀" -> "㇀",
      "㇁" -> "㇁",
      "㇂" -> "㇂",
      "㇃" -> "㇃",
      "㇄" -> "㇄",
      "㇅" -> "㇅",
      "㇆" -> "㇆",
      "㇇" -> "㇇",
      "㇈" -> "㇈",
      "㇉" -> "㇉",
      "㇊" -> "㇊",
      "㇋" -> "㇋",
      "㇌" -> "㇌",
      "㇍" -> "㇍",
      "㇎" -> "㇎",
      "㇏" -> "㇏",
      "㇐" -> "㇐",
      "㇑" -> "㇑",
      "㇒" -> "㇒",
      "㇓" -> "㇓",
      "㇔" -> "㇔",
      "㇕" -> "㇕",
      "㇖" -> "㇖",
      "㇗" -> "㇗",
      "㇘" -> "㇘",
      "㇙" -> "㇙",
      "㇚" -> "㇚",
      "㇛" -> "㇛",
      "㇜" -> "㇜",
      "㇝" -> "㇝",
      "㇞" -> "㇞",
      "㇟" -> "㇟",
      "㇠" -> "㇠",
      "㇡" -> "㇡",
      "㇢" -> "㇢",
      "㇣" -> "㇣"
    )
  }

  def loadIds() = {
    var tempLines = new ListBuffer[List[String]]() //List[List[String]] = List()
    //var tempHash: Map[String, String] = Map();
    val source = Source.fromFile("./src/resources/ids.txt")

    for (eachLineElem: String <- source.getLines()) {
      if (eachLineElem != null && eachLineElem.size > 1) {
        tempLines += eachLineElem.split("\\s+").toList
      }
    }

    //tempLines.toList.foreach(x => idsLinesMap + (x(1) -> x(2)))
    idsRawLines = tempLines.toList

    for (eachSubList <- idsRawLines) {
      if (eachSubList != null
        && eachSubList.size > 2
        && eachSubList(1) != null
        && eachSubList(1).size > 0
        && eachSubList(2) != null
        && eachSubList(2).size > 0) {
        idsLinesMap = idsLinesMap + (eachSubList(1) -> eachSubList(2).replaceAll("[A-Z\\[\\]]", ""))
      }
    }
    //for (eachLine <- idsRawLines) {
    //  if (eachLine.size < 3) {
    //    println(eachLine.toString())
    //  }
    //}

    //idsLinesMap = tempHash
    source.close()
  }



  private val zmA = HashMap(
    "㐬" -> "1PAa 2PSzs 2PXnd",//流official vszn  compare 統  official zszr
    //but in ids it is always a zs execpt in 流
    "丐" -> "1PAa 2PAi 1PXz", //"丏" -> "1Pa 1Pz 1Py",//,眄//
    "与" -> "1PAa 1PXz 1PAa", "平" -> "1PAa 2SIua",
    "疌" -> "1PAa 2SIxb 2SSii", "丈" -> "1PAa 2PSos", //捷
    "末" -> "1PAa 1PSf", "于" -> "1PAa 1SId",//宇
    "豆" -> "1PAa 1PAj 2PAua", "㥑" -> "1PAa 2PAnk 2PXww 2PSwz",//優
    "一" -> "1PAa", "丁" -> "2PIai", "㇀" -> "1TAa",
      "瓦" -> "1PAa 1PXz 1PXy 1PSs")
  private val zmB = HashMap(
    "元" -> "2PAbd 2PXrd", "⿻二丨" -> "2SIbi",//完 舉
    "亙" -> "2PAbd 2TSrs", "𠁁" -> "2SAbz", "壴" -> "1SAb 1PAj 2PAua",//恆 斲 喜
    "未" -> "2PAbd 2PSko", "井" -> "2PAbd 2SInd",
    "五" -> "2PAbi 1PXx", "赤" -> "1PAb 2SInd 1PSo",
    "土" -> "1PAb", "士" -> "1SAb", "二" -> "2PAbd", "示" -> "2PSbk",
    "走" -> "2PSbo", "耂" -> "2PMbm", "者" -> "2SAbm", "工" -> "2PAbi","亞" -> "2PAbz")
  private val zmC = HashMap(
    "王" -> "1PAc", "班" -> "1PAc 2SMud 1PAc", "𡗗" -> "2PSco",
    "三" -> "2PAcd", "玉" -> "2PScs", "耳" -> "2PIce", "馬" -> "2PScu",
    "髟" -> "2PMch", "長" -> "2SSch", "镸" -> "2SSch", "丰" -> "2PIci", "龶" -> "2SAci",
    "豐" -> "2SAci", "耒" -> "2PSck", "鬥" -> "2PIcc", "𡗗" -> "2PSco", "春" -> "2SAco")
  private val zmD = HashMap(
    "才" -> "1SId 1PMm",
    "事" -> "1SId 1PAj 2PIxb",
    "扌" -> "1PAd", "寸" -> "2PSds")

  private val zmE = HashMap(
    "干" -> "1PAa 2PIed", "艹" -> "1TAe", //繭
    "𠀎" -> "2SAeb", "世" -> "1PAe 1PAa 1PXz", "甚" -> "2PSec 1PXz",
    "斗" -> "2SStd 2PIed", "華" -> "1PAea 1PIe 2SIbi",
    "卌" -> "1TIe", "艹" -> "1PIe", "廾" -> "1SIe", "十" -> "2PIed", "革" -> "2PIee",
    "廿" -> "2PAea", "龷" -> "2SAea", "卌一" -> "2SAea", "帶" -> "2SXea 2PXww 2PIll",
    "甘" -> "2PAeb", "其" -> "2PSec", "栽" -> "2SSeh", "𢦏" -> "2PSeh")
  private val zmF = HashMap(
    "𣎳" -> "1PXf", "襾" -> "2TAfj",
    "柬" -> "1PSf 2SlAk", "朿" -> "1PSf 2PXld", "惠" -> "2SSfk 2PSwz",
    "本" -> "1PSf 1PAa", "東" -> "1PSf 1PAk", "束" -> "1PSf 1PAj",
    "木" -> "1PSf", "朩" -> "1PSf", "酉" -> "2PAfd", "覀" -> "2PAfj", "西" -> "2SAfj",
    "車" -> "2PIfk", "專" -> "2SSfk 2SSds", "甫" -> "2PSfb", "雨" -> "2PSfv")
  private val zmG = HashMap(
    "存" -> "2SMgd 1PIi 2PAya", "乑" -> "2SSgq", "尤" -> "2PSgr",//就
    //存  眾--乑=2Pgq "眾" -> "2Plk 1Pm 1Ps 2Pod 2Pod" //⿱取乑 聚cexg
    "犬" -> "2PSgd 1PSs", "面" -> "1SMg 2PAjd 2SAkd",
    "石" -> "1PAg", "丆" -> "1SMg", "厂" -> "2PMgg", "大" -> "2PSgd",
    "𠂇" -> "2SMgd", "辰" -> "2PSgh", "尨" -> "2SSgm", "不" -> "2PSgi", "頁" -> "2SSgo",
    "而" -> "2PIgl", "豕" -> "2PSgq", "𧰨" -> "2SSgq",
    "尢" -> "2SXgr", "兀" -> "2SXgr" //光 --ids: koar
    , "在" -> "2SMgd 1PIi 1PAb")
  private val zmH = HashMap(
    "巨" -> "1PXh 1SAx", "𦣞" -> "1SXh",//熙
    "戊" -> "2SShm", "旡" -> "1PXh 2PXrd", "戉" -> "2PShm 1PXz",
    "匚" -> "1PXh", "臣" -> "1SXh", "一⿰𠄌⿺乀丿" -> "1SSh", "⿰𠄌⿺乀丿" -> "1SSh", //展
    "七" -> "2PXhd", "巠" -> "2PXhd 2PAbi", "弋" -> "2PShs", "戈" -> "2PShm", "戊" -> "2SShm",
    "牙" -> "2PMhi", "至" -> "2PAhb") //𠥫
  private val zmI = HashMap(
    "⿴卝丨" -> "1PIi 1PIi 1PAa 1PIi 1PAa", //羋
    "𢇍" -> "1PXz 2PSzs 1PXz 2PSzs 1PAa 1PXz 2PSzs 1PXz 2PSzs 1PIi 1PAa", // 躖
    "凸" -> "1PIi 1PAa 1PIi 1PXy 1PAa", "凹" -> "1PIi 1PXy 1PIi 1PXx 1PAa",
    "壺" -> "1SAb 2PXww 1PIi 1PAa 1PXz 1PXz 1PIi 1PAa 1PIi 1PAa", //sammenlign 亞bz
    "丨" -> "1PIi", "亅" -> "1PIi", "虛" -> "2PXih 2TAku",
    "虫" -> "1PSi", "卜" -> "2PSid", "⺊" -> "2PAid", "乍" -> "2PAma 2SAid",
    "虍" -> "2PXih", "虎" -> "2SXih",
    "止" -> "2PAii", "延" -> "1SSy 1PMm 2PAii", //"⿱丿③" -> "1Pm 2Pii", //延
    "龰" -> "2SSii", "齒" -> "2SIio")
  private val zmJ = HashMap(
    "囙" -> "2PAjd 1SAx", "呂" -> "1PAj 2SAnj", //宮
    "口" -> "1PAj", "囗" -> "2PAjd", "因" -> "2SAjd", "足" -> "2PSji", "𧾷" -> "2SAji")
  private val zmK = HashMap(
    "曳" -> "1PAk 1PXz 1PMm", "里" -> "1PAk 1PAb", "⿻甲一" -> "1PAk 2PIed", //洩 單
    "曲" -> "1PAk 2SIkd", "禺" -> "1PAk 2PXl 2SSzs", "畢" -> "1PAk 1PIe 2SIbi",
    "日" -> "1PAk", "曰" -> "1SAk", "⿻口一" -> "1SAk", "刂" -> "2PIkd", "业" -> "2PAku",
    "業" -> "2SSku", "婁" -> "2PAkj 2PAzm", "非" -> "2PAkc", "小" -> "2PSko", "⺌" -> "2SMko",
    "𣥂" -> "2SMko", "水" -> "2PSkv", "氺" -> "2SSkv", "㡀" -> "2SSkv 2PXld", "眔" -> "2PAlk 2SSkv",
    "田" -> "2PAki", "由" -> "2PAkia", "甲" -> "2PIkib", "申" -> "2PIkic")
  private val zmL = HashMap(
    "崋" -> "2PIll 1PAa 2TIe 2SIbi", "𡚇" -> "2PAlk 1PIi 1PAa 1PXz 2PAlk 2PSgd",//嬽
    "典" -> "2PXld 2SAea 1PSo", "鼎" -> "1PAl 2SMzi 2SXnx", "冊" -> "2PXld 1PIe",
    //"眾" -> "2Plk 1Pm 1Ps 2Pod 2Pod",
    //眾--乑=2Pgq "眾" -> "2Plk 1Pm 1Ps 2Pod 2Pod"
    "冉" -> "2PXld 1PAb", "央" -> "2PXld 2PSgd", "⺆" -> "2PXld",//調
    "黑" -> "2SAlk 1PAb 2SSuo", "肉" -> "2PXld 2PSod 2PSod",
    "目" -> "1PAl", "冂" -> "2PXld", "同" -> "2SXld 1PAa 1PAj", "𠔼" -> "2SXld 1PAa",
    "冋" -> "2SXld 1PAj", "冏" -> "2SXld 2PXrd 1PAj", "⿵冂𢆉" -> "2SXld 2PAua 2PIed" ,
    "用" -> "2SXld 2SIbi",
    "甬" -> "2SSxs 2SXld 2SIbi", "角" -> "1SXr 2SXld 1PIb", "𠕁" -> "2SXld 1SIe",
    "岡" -> "2SXld 2PAua 2PIll",
    "巾" -> "2PXli", "山" -> "2PIll", "罒" -> "2PAlk", "四" -> "2SAlk", "曾" -> "2PMud 2SAlk 1PAk",
    "會" -> "1PSod 1PSs 2SAlk 1PAk", "皿" -> "2PAlka", "且" -> "2PAlc", "⿴且一" -> "2SAlc",
    "貝" -> "2SSlo", "咼" -> "2PAlj", "見" -> "2SXlr", "骨" -> "2PAlw")
  private val zmM = HashMap(
    //𣏈 m shape - single stroke
    "禹" -> "1PMm 1PSi 2PXld", "㇓" -> "1PMm",//"凞" -> "2Ptd 1Pm 1Sh ",//㇓
    "熏" -> "1PMm 2PIed 2SAlk 1PAb 2SSuo",//勳 熏(mebu, ml, mlu)
    "𦈢" -> "2PAma 2PIed 2PAii ", //卸maiy 𦈢(maai, maei, mbii)
    "垂" -> "1PMm 2PIed 1PIe 1PAb", "朱" -> "1PMm 2PAbd 2PSko", //睡
    "年" -> "2PAma 2SImb", "重" -> "1PMm 2PIed 1PAk 1PAb", "丿" -> "1PMm",
    "竹" -> "1PIm", "⿱𠂉丶" -> "1SSm", "舌" -> "2PAmi", "𠂉" -> "2PAma",
    "矢" -> "2SSma", "攵" -> "2PSmo", "牛" -> "2PImb", "失" -> "2SAmb 2PSod",
    "𠂒" -> "2SAmb", "㐄" -> "2SImb", "气" -> "2PXmy", "氣" -> "2SSmy", "毛" -> "2PXmh",
    "禾" -> "2PSmf", "余" -> "2PSod 2SSmf", "生" -> "2PAmc", "手" -> "2PImd",
    "龵" -> "2SMmd", "千" -> "2PIme",
    "缶" -> "2PAma 2PIed 2PIzi", "我" -> "2SAmd 2PShm")
  private val zmN = HashMap(
    "盥" -> "2SAnb 2PSkv 3PAlka", //"與" -> "",
    "段" -> "2TAnc 2PXqd 2PSxs", "雋" -> "2PAni 1PIi 1PXy 1PIi 1PXy",
    "亻" -> "1PIn", "片" -> "2PXnx",
    "川" -> "2PInd", "⿰丿丨" -> "2SInd", "⿲丿二丨" -> "2SInd", "巟" -> "2PXsh 2PXnd",//齊
    "𣶒" -> "2SAnd", "臼" -> "2PAnb", "臼丨" -> "2SInb",
    "與" -> "2SAnb 2TIid 1PSo", "𦥯" -> "2SAnb 2PSos 2PSos", "𦥑冖" -> "2SXnb",
    "鬼" -> "2PSnj", "⿻白丿" -> "2SMnj", "⿱丿囗" -> "2SAnj",
    "白" -> "2PAnk", "自" -> "2PAnl", "⿱丿冂" -> "2SXnl", "隹" -> "2PAni", "身" -> "2PMnc")
  private val zmO = HashMap(
    "史" -> "1PAj 2PSos", "谷" -> "1PSo 2PSod 1PAj",//聲 bxqc
    "八" -> "1PSo", "⿱丿丿" -> "2PMod", "人" -> "2PSod",
    "入" -> "2PModa", "乂" -> "2PSos", "㐅" -> "2PSos", "⿻⿱丿丿丶" -> "2SSos",
    "𠚍" -> "2SSos 2PIzi", "彳" -> "2PIoi", "行" -> "2SIoi",
    "食" -> "2PSox", "飠" -> "2SSox",
  //oi characters - among the first 6000
  //術,徵,衛,衝,街,衡,衍,銜,衙,愆,衢
    "術" -> "2SIoi 1PSf 1PSs", "徵" -> "2SIoi 2PIll 1PAa 1PMm 1PAb 1PSmo",
    "衛" -> "2SIoi 1SAx 1PAj 2SImb", "衝" -> "2SIoi 1PMm 2PIed 1PAk 1PAb",
    "街" -> "2SIoi 1PAb 1PAb", "衡" -> "2SIoi 1SAr 2PSgd", "衍" -> "2SIoi 1PAv",
    "銜" -> "2SIoi 1PAp", "衙" -> "2SIoi 1PAa 1SAx 1PAj",
    "愆" -> "2SIoi 1PAv 2PSwz", "衢" -> "2SIoi 1PAl 1PAl 2PAni"
  )
  private val zmP = HashMap(
    //"爲" -> "",//儰 蟡
    "𠂢" -> "2SMpd 2TSrh", "㐆" -> "2SMpd 2PAxb 1PXy",//派
    "金" -> "1PAp", "彡" -> "2PMpd", "斤" -> "2PIpd", "𠂆" -> "2SMpd",
    "丘" -> "2PApda", "豸" -> "2PMpq", "瓜" -> "2PSps", "釆" -> "2PSpf", "采" -> "2SMpv 1PSf",
    "爪" -> "2PSpv", "爫" -> "2SMpv", "舟" -> "2PSpy")
  private val zmQ = HashMap(
    "册" -> "1SXq 1SXq 1PAa", //珊
    "𠘧" -> "2PXqd", "月" -> "1PAq", "⿵⺆冫" -> "1PAq", //能
    "丹" -> "1SXq 1SSs", "几" -> "2PXqd", "巩" -> "2PAbi 2SSqd",
    "凡" -> "2PSqda", "風" -> "2PSqi", "九" -> "2PXqy", "丸" -> "2PSqya", "殳" -> "2PSqx",
    "犭" -> "2PMqm")
  private val zmR = HashMap(
    "⿴𠂊冫" -> "2SArs", "象" -> "1PXr 1PAj 2SSgq",
    "裊" -> "2TXrz 1SAs 2SSrh", "梟" -> "2TXrz 1PSf",
    "卿" -> "2SMrh 2SSxo 1SIy", "兜" -> "2SXrh 2PAnk 1SAx 2PXrd",
    "旅" -> "2PXsy 2PAma 2SSrh", "玈" -> "1SAs 1PXz 2PSzs 2PAma 2SSrh",
    "祣" -> "2PSws 2PAma 2SSrh", "夊" -> "2SSrs", //傻
    "印" -> "2SXrh 1PAa 1SIy", "⿰⿴𠂊冫②" -> "2SSrs", //祭
    "𠂊" -> "1SXr", "⿰③刀" -> "2SXrh 1PSs 2PMyd",//劉
    "免" -> "1SXr 1PAj 2PXrd",
    "角" -> "1SXr 1SXl 1PId", "𩵋" -> "1SAr 2PSgd",
    "魚" -> "1SSr", "久" -> "1SXr 1PSs",
    "儿" -> "2PXrd", "匕" -> "2PXrr", "比" -> "2PXrr 2PXrr", "𠤎" -> "2SXrr",
    "勹" -> "2PXry", "包" -> "2SXry", "匃" -> "2SXry",
    "夕" -> "2PSrs", "夂" -> "2SSrs", "⿴𠂊⺀" -> "2SSrs",
    "氏" -> "2PSrh", "𠂎" -> "2SMrh", "卬" -> "2SXrh 1SIy", "𧘇" -> "2SSrh",
    "欠" -> "2PSro", "鳥" -> "2SSrz", "島" -> "2SXrz 2PIll", "烏" -> "2SSrza")
  private val zmS = HashMap(
    "主" -> "1PAs 1PAc",
    "乀" -> "1PSs", "褎" -> "1SAs 2SXrh 1PAa 2PSmf 2SSrh",
    "亦" -> "1SAs 2SInd 1PSo", "⿱丶⑤" -> "1PSs 2PSxo", //郎
    "言" -> "1PAs", "亠" -> "1SAs", "文" -> "2PSso", "亡" -> "2PXsh", //compare "㐬" -> "a zs nd",
    "立" -> "2PAsu", "辛" -> "2PIse", "方" -> "2PXsy",
    "永" -> "2PSsk", "龍" -> "2PAsi",
  "丶" -> "1PSs")
  private val zmT = HashMap(
    "廌" -> "2SAtx 1PAa 1PXz 2SSuo", //etzu
    "疒" -> "1PMt", "病" -> "1SSt", "冫" -> "2PAtd", "⺀" -> "2SStd",
    "北" -> "2SAti 2PXrr", "广" -> "2PMtg", "廣" -> "2SStg", "鹿" -> "2PXtx",
    "⿸广⿻コ⿰丨丨" -> "2SAtx")
  private val zmU = HashMap(
    //chars that didnt have a shape breakdown:
    "羲" -> "2SAuc 2PSmf 1PAa 1PXz 2PShm", "儰" -> "1PIn 1PIe 2SMud 1PXy 2SSuo",//犧
    "為" -> "2SMud 1PXy 2SSuo", "蟡" -> "1PS1 2SMud 1PXy 2SSuo",
    "撝" -> "1PAd 2SMud 1PXy 2SSuo",
    "噅" -> "1PAj 2SMud 1PXy 2SSuo", "𤾡" -> "2PAnk 2SMud 1PXy 2SSuo", //"蘤"
    "辨" -> "2PIse 2SMud 2PIse", "寪" -> "2PXwd 2SMuo 1PXy 2SSuo",
    "鄬" -> "2SMuo 1PXy 2SSuo 1PXy"
    ,"关" -> "2PAua 2PSgd" //送
    , "忄" -> "1PSu", "㣺" -> "1SSu", "米" -> "2PSuf", "丷" -> "1SMud",
    "②又" -> "1SMud 2PSxs"//𡉢 帰
    , "②帚" -> "1SMud 2PAxb 2PXww 2PIli", "丷下" -> "2PAua 2PSid", "䒑" -> "2PAua"
    , "乎" -> "1PMm 2SIua", "⿻干丷" -> "1PAa 2SIua" //平
    , "火" -> "2PSuo", "灬" -> "2SSuo"//灰 𤇴
    , "半" -> "2PIub", "𢆉" -> "2SIub", "龹" -> "2SSub"//伴 㚔 堘 (2 missing)
    , "⿱䒑⿻二丨" -> "2PIuc", "𦍌" -> "2SAuc"//𡗗 差 𦍼 𢜗 𦍌 羊  𩱁
  )
  private val zmV = HashMap(
    "州" -> "2PSvd 2PInd",
    "氵" -> "1PAv", "⿻一氺" -> "1SId 1SSv", "⿱丷八" -> "1SSv"//𢯼 求 剑 𠦂
     , "佥" -> "2PSod 2PAbd 2PMvd", "兆" -> "1SSv 2PXrd"
  )
  private val zmW = HashMap(
    "之" -> "1PSw", "辶" -> "1SSw", "令" -> "2PSod 1SSw", "礻" -> "2PSws",
    "宀" -> "2PXwd", "定" -> "2SSwd", "冖" -> "2PXww"//之 辶 令 宀 冖 定
    , "穴" -> "2PSwo", "冘" -> "2PXww 2PXrd"
  , "衤" -> "2PSwt"
    , "戶" -> "2SMpd 1SAx" //"戶" -> "2Pwm" //𫉚 戶
   , "心" -> "2PSwz"//心 忠
   , "黽" -> "2PXwx", "龜" -> "1PXr 2Swx" //澠 龜
  )
  private val zmX = HashMap(
    "臦" -> "1SXx 1PIi 1PAa 1PXz 1PIi 1SXh",
    "丑" -> "1SXx 2PAed", "𤴔" -> "1PXx 2PAii", "鵖" -> "2SSxo 2SSrz", //羞 疏
    "肅" -> "2SAxb 2SInd", "尺" -> "1PMxm 1PSs", "𡬠" -> "2SSxo 2PSds",
    "彐" -> "2PAxb", "⿻コ一" -> "2PAxb", //寢
    "承" -> "1PXx 2PSkv 2PAcd", "⿻⿻コ一④" -> "2PSxk", //尋 兼
    "隶" -> "2SIxb 2SSkv", "尹" -> "3PMxma",
    "疋" -> "1PXx 2SSii", "⿻⿻コ一亅" -> "2SIxb", //爭 君
    "叚" -> "1SAx 2PIbd 1SAx 2PSxs", "既" -> "2SSxo 1PXh 2PXrd",
    "即" -> "2SSxo 1SIy", "⿳𦥑一" -> "2SAnb", "龴" -> "2SSxs", "乛" -> "1PXx", //興
    "廄" -> "2PMtg 2SSxo 2PSqx",
    "皮" -> "2PSxi", "𠃜" -> "2SMxm", "𫝀" -> "1SAx", //韋 xjmb 衛 oijm ??? - ser ud til at det sidste element er glemt
    "癶" -> "2SSxs", "尸" -> "2PMxm",
    "𠃍" -> "1PXx", "㇇" -> "1PXx", "肀" -> "2SIxb", "ユ" -> "1SAx",
    "艮" -> "2PSxo", "⿱𦘒一" -> "2SAxb",
    "⿴尸一" -> "2SMxm",
    "幺" -> "1PXz 2PSzs",
    "門" -> "2PAxd",//⿵門口
    "又" -> "2PSxs"
  )

  //衛 𫝀

  private val zmY = HashMap(
    "⿰丿𠃌" -> "2SMym", "刃" -> "2PMyd 1PSs", //別 認
    "乜" -> "1PXy 1PXz", "⿱勹又" -> "2SXym 2PSxs", "⿻刀二" -> "1PMy 2SAbi", //沒vyxs 那yby "1Pyd 2Sbi"
    "卍" -> "1PXy 1PIi 1PAa 1PIi", "孑" -> "1TIy 1TAa", "孓" -> "1TIy 1PSs",
    "⿲刀丫③" -> "1PMy 2PMud 1PIi 2SSrh", "㔾" -> "2SXyyb", //齊 犯
    "民" -> "3SXyyb 2SShd", "及" -> "1TXy 1PMm 1PSs", "飛" -> "3SSyda 1SSv 3SSyda 1SSv 1PMm 2SInd",
    "𠃌" -> "1PXy", "㠯" -> "1SAy", "廴" -> "1SSy", "卩" -> "1PIy",
    "阝" -> "1PIy", "了" -> "1PIy", "刀" -> "2PMyd", "乙" -> "2PXyda", "𠄎" -> "1PXy",
    "⺄" -> "2SSyda", "也" -> "2PXyi", "巴" -> "2PXyia", "子" -> "2PAya",
    "力" -> "2PMym", "⿻丿𠃌" -> "2SMym", "己" -> "2PXyy", "已" -> "2PXyya",
    "巳" -> "2PXyyb", "弓" -> "2PXyz", "𢎨" -> "2SMyz", "习" -> "2PAyt")
  private val zmZ = HashMap(
    "焉" -> "1PAa 2PAii 1PAa 1PXz 2SSuo",
    "巤" -> "2PXzd 2PAjd 2PSos 2SSzd", //獵
    "𠃑" -> "1PXz", "㇉" -> "1PXz", "𠃊" -> "1PXz", //,"𢆸" -> "1Pz 2Pzs 1Pz 2Pzs 1Pz",//斷
    "乡" -> "1PXz 2SMzm", "夨" -> "1PXz 1PAa 1PSs",//奊
    "糹" -> "1SXz", "糸" -> "1SSz", "毌" -> "2SAzy", "丱" -> "2SIzi",
    "乚" -> "1PXz",
    "以" -> "1PXz 1PSs 2PSod" //denne har ikke et ids opslag
  //z single stroke - 𣭖
  //mangle 1Pz. maaske fordi den kun findes som simplificeret
  //𠀔 𠫔 𣦶 𠀅 𢻽 𣨒 㙲
  , "䜌" -> "2PSzs" //𦣏
  , "巛" -> "2PXzd"
  , "巜" -> "2SXzd"
  , "鼠" -> "2PAnb 2SSzd" //𡿬 𤰕 𣜌 鼠 𤢪
  //, "𤢪" -> skal tilfoejes hvis noedvendigt
  , "厶" -> "2PSzs"
  //厶 勾 𠇇
  , "女" -> "2PAzm"
  , "互" -> "2PAbd 2SXzm"
  , "彑" -> "2SAzm"
  , "𠂈" -> "2SMzm"
  //𠯆 𠯞 彑
  , "母" -> "2PSzy"
  , "毋" -> "2SMzy" //der mangler en 2Szy form
  //每 毒 𩬍 毋
  , "爿" -> "2SMzi"
  , "凵" -> "2PIzi"
  , "丩" -> "2SIzi"
  , "屮" -> "2SIzi"
  , "艸" -> "2SIzi"
  //𤖀 𠁫 𠇿 𡴅 𦱶 𡴳 -venstre side af Ding mangler
  )

  var zmMap: Map[String, String] =
    zmA ++ zmB ++ zmC ++ zmD ++ zmE ++ zmF ++
      zmG ++ zmH ++ zmI ++ zmJ ++ zmK ++ zmL ++
      zmM ++ zmN ++ zmO ++ zmP ++ zmQ ++ zmR ++
      zmS ++ zmT ++ zmU ++ zmV ++ zmW ++ zmX ++
      zmY ++ zmZ

  def getZmMap() : Map[String, String] = zmMap
}

//source.getLines().toList.foreach { elem =>
//      var valueStr: String = ""
//      var eachLine: List[String] = null
//      try {
//        if (elem.size > 3) {
//          eachLine = elem.split("\\s+").toList
//          if (eachLine != null && eachLine.size > 2) {
//            valueStr = eachLine(2).replaceAll("[\\x00-\\x7F]", "")
//            if (eachLine.nonEmpty && valueStr.nonEmpty && valueStr.size > 0) {
//              tempHash + (eachLine(1) -> valueStr)
//            }
//          }
//        }
//      }catch {
//
//         case NonFatal(t) => {println(valueStr + " " + eachLine.toString())}
//        //Exception => {println(valueStr + " " + eachLine.toString())}
//        //throw new Exception(valueStr + " " + eachLine.toString())
//        //case _: => Throwable => println(valueStr + " " + eachLine.toString())
//        }
