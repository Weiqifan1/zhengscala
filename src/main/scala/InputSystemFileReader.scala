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
    "㐬" -> "1Pa 2Pzs 2Pnd",//流official vszn  compare 統  official zszr
    //but in ids it is always a zs execpt in 流
    "丐" -> "1Pa 2Pi 1Pz", //"丏" -> "1Pa 1Pz 1Py",//,眄//
    "与" -> "1Pa 1Pz 1Pa", "平" -> "1Pa 2Sua",
    "疌" -> "1Pa 2Sxb 2Sii", "丈" -> "1Pa 2Pos", //捷
    "末" -> "1Pa 1Pf", "于" -> "1Pa 1Sd",//宇
    "豆" -> "1Pa 1Pj 2Pua", "㥑" -> "1Pa 2Pnk 2Pww 2Pwz",//優
    "一" -> "1Pa", "丁" -> "2Pai", "㇀" -> "1Ta",
      "瓦" -> "1Pa 1Pz 1Py 1Pz")
  private val zmB = HashMap(
    "元" -> "2Pbd 2Prd", "⿻二丨" -> "2Sbi",//完 舉
    "亙" -> "2Pbd 2Trs", "𠁁" -> "2Sbz", "壴" -> "1Sb 1Pj 2Pua",//恆 斲 喜
    "未" -> "2Pbd 2Pko", "井" -> "2Pbd 2Snd",
    "五" -> "2Pbi 1Px", "赤" -> "1Pb 2Snd 1Po",
    "土" -> "1Pb", "士" -> "1Sb", "二" -> "2Pbd", "示" -> "2Pbk",
    "走" -> "2Pbo", "耂" -> "2Pbm", "者" -> "2Sbm", "工" -> "2Pbi","亞" -> "2Pbz")
  private val zmC = HashMap(
    "王" -> "1Pc", "班" -> "1Pc 2Sud 1Pc", "𡗗" -> "2Pco",
    "三" -> "2Pcd", "玉" -> "2Pcs", "耳" -> "2Pce", "馬" -> "2Pcu",
    "髟" -> "2Pch", "長" -> "2Sch", "镸" -> "2Sch", "丰" -> "2Pci", "龶" -> "2Sci",
    "豐" -> "2Sci", "耒" -> "2Pck", "鬥" -> "2Pcc", "𡗗" -> "2Pco", "春" -> "2Sco")
  private val zmD = HashMap(
    "才" -> "1Sd 1Pm",
    "事" -> "1Sd 1Pj 2Pxb",
    "扌" -> "1Pd", "寸" -> "2Pds")

  private val zmE = HashMap(
    "干" -> "1Pa 2Ped", "艹" -> "1Te", //繭
    "𠀎" -> "2Seb", "世" -> "1Pe 1Pa 1Pz", "甚" -> "2Pec 1Pz",
    "斗" -> "2Std 2Ped", "華" -> "1Pe 1Pa 1Pe 2Sbi",
    "卌" -> "1Te", "艹" -> "1Pe", "廾" -> "1Se", "十" -> "2Ped", "革" -> "2Pee",
    "廿" -> "2Pea", "龷" -> "2Sea", "卌一" -> "2Sea", "帶" -> "2Sea 2Pww 2Pll",
    "甘" -> "2Peb", "其" -> "2Pec", "栽" -> "2Peh", "𢦏" -> "2Seh")
  private val zmF = HashMap(
    "𣎳" -> "1Pf", "襾" -> "2Tfj",
    "柬" -> "1Pf 2Slk", "朿" -> "1Pf 2Pld", "惠" -> "2Sfk 2Pwz",
    "本" -> "1Pf 1Pa", "東" -> "1Pf 1Pk", "束" -> "1Pf 1Pj",
    "木" -> "1Pf", "朩" -> "1Pf", "酉" -> "2Pfd", "覀" -> "2Pfj", "西" -> "2Sfj",
    "車" -> "2Pfk", "專" -> "2Sfk 2Sds", "甫" -> "2Pfb", "雨" -> "2Pfv")
  private val zmG = HashMap(
    "存" -> "2Sgd 1Pi 2Pya", "乑" -> "2Sgq", "尤" -> "2Pgr",//就
    //存  眾--乑=2Pgq "眾" -> "2Plk 1Pm 1Ps 2Pod 2Pod" //⿱取乑 聚cexg
    "犬" -> "2Pgd 1Ps", "面" -> "1Sg 2Pjd 2Skd",
    "石" -> "1Pg", "丆" -> "1Sg", "厂" -> "2Pgg", "大" -> "2Pgd",
    "𠂇" -> "2Sgd", "辰" -> "2Pgh", "尨" -> "2Sgm", "不" -> "2Pgi", "頁" -> "2Sgo",
    "而" -> "2Pgl", "豕" -> "2Pgq", "𧰨" -> "2Sgq",
    "尢" -> "2Sgr", "兀" -> "2Sgr" //光 --ids: koar
    , "在" -> "2Sgd 1Pi 1Pb")
  private val zmH = HashMap(
    "巨" -> "1Ph 1Sx", "𦣞" -> "1Sh",//熙
    "戊" -> "2Shm", "旡" -> "1Ph 2Prd", "戉" -> "2Phm 1Pz",
    "匚" -> "1Ph", "臣" -> "1Sh", "一⿰𠄌⿺乀丿" -> "1Sh", "⿰𠄌⿺乀丿" -> "1Sh", //展
    "七" -> "2Phd", "巠" -> "2Phd 2Pbi", "弋" -> "2Phs", "戈" -> "2Phm", "戊" -> "2Shm",
    "牙" -> "2Phi", "至" -> "2Phb") //𠥫
  private val zmI = HashMap(
    "⿴卝丨" -> "1Pi 1Pi 1Pa 1Pi 1Pa", //羋
    "𢇍" -> "1Pz 2Pzs 1Pz 2Pzs 1Pa 1Pz 2Pzs 1Pz 2Pzs 1Pi 1Pa", // 躖
    "凸" -> "1Pi 1Pa 1Pi 1Py 1Pa", "凹" -> "1Pi 1Py 1Pi 1Px 1Pa",
    "壺" -> "1Sb 2Pww 1Pi 1Pa 1Pz 1Pz 1Pi 1Pa 1Pi 1Pa", //sammenlign 亞bz
    "丨" -> "1Pi", "亅" -> "1Pi", "虛" -> "2Pih 2Tku",
    "虫" -> "1Pi", "卜" -> "2Pid", "⺊" -> "2Pid", "乍" -> "2Pma 2Sid",
    "虍" -> "2Pih", "虎" -> "2Sih",
    "止" -> "2Pii", "延" -> "1Sy 1Pm 2Pii", //"⿱丿③" -> "1Pm 2Pii", //延
    "龰" -> "2Sii", "齒" -> "2Sio")
  private val zmJ = HashMap(
    "囙" -> "2Pjd 1Sx", "呂" -> "1Pj 2Snj", //宮
    "口" -> "1Pj", "囗" -> "2Pjd", "因" -> "2Sjd", "足" -> "2Pji", "𧾷" -> "2Sji")
  private val zmK = HashMap(
    "曳" -> "1Pk 1Pz 1Pm", "里" -> "1Pk 1Pb", "⿻甲一" -> "1Pk 2Ped", //洩 單
    "曲" -> "1Pk 2Skd", "禺" -> "1Pk 2Pl 2Szs", "畢" -> "1Pk 1Pe 2Sbi",
    "日" -> "1Pk", "曰" -> "1Sk", "⿻口一" -> "1Sk", "刂" -> "2Pkd", "业" -> "2Pku",
    "業" -> "2Sku", "婁" -> "2Pkj 2Pzm", "非" -> "2Pkc", "小" -> "2Pko", "⺌" -> "2Sko",
    "𣥂" -> "2Sko", "水" -> "2Pkv", "氺" -> "2Skv", "㡀" -> "2Skv 2Pld", "眔" -> "2Plk 2Skv",
    "田" -> "2Pki", "由" -> "2Pkia", "甲" -> "2Pkib", "申" -> "2Pkic")
  private val zmL = HashMap(
    "崋" -> "2Pll 1Pa 2Te 2Sbi", "𡚇" -> "2Plk 1Pi 1Pa 1Pz 2Plk 2Pgd",//嬽
    "典" -> "2Pld 2Sea 1Po", "鼎" -> "1Pl 2Szi 2Snx", "冊" -> "2Pld 1Pe",
    //"眾" -> "2Plk 1Pm 1Ps 2Pod 2Pod",
    //眾--乑=2Pgq "眾" -> "2Plk 1Pm 1Ps 2Pod 2Pod"
    "冉" -> "2Pld 1Pb", "央" -> "2Pld 2Pgd", "⺆" -> "2Pld",//調
    "黑" -> "2Slk 1Pb 2Suo", "肉" -> "2Pld 2Pod 2Pod",
    "目" -> "1Pl", "冂" -> "2Pld", "同" -> "2Sld 1Pa 1Pj", "𠔼" -> "2Sld 1Pa",
    "冋" -> "2Sld 1Pj", "冏" -> "2Sld 2Prd 1Pj", "⿵冂𢆉" -> "2Sld 2Pua 2Ped" , "用" -> "2Sld 2Sbi",
    "甬" -> "2Sxs 2Sld 2Sbi", "角" -> "1Sr 2Sld 1Pb", "𠕁" -> "2Sld 1Se", "岡" -> "2Sld 2Pua 2Pll",
    "巾" -> "2Pli", "山" -> "2Pll", "罒" -> "2Plk", "四" -> "2Slk", "曾" -> "2Pud 2Slk 1Pk",
    "會" -> "1Pod 1Ps 2Slk 1Pk", "皿" -> "2Plka", "且" -> "2Plc", "⿴且一" -> "2Slc",
    "貝" -> "2Slo", "咼" -> "2Plj", "見" -> "2Slr", "骨" -> "2Plw")
  private val zmM = HashMap(
    //𣏈 m shape - single stroke
    "禹" -> "1Pm 1Pi 2Pld", "㇓" -> "1Pm",//"凞" -> "2Ptd 1Pm 1Sh ",//㇓
    "熏" -> "1Pm 2Ped 2Slk 1Pb 2Suo",//勳 熏(mebu, ml, mlu)
    "𦈢" -> "2Pma 2Ped 2Pii ", //卸maiy 𦈢(maai, maei, mbii)
    "垂" -> "1Pm 2Ped 1Pe 1Pb", "朱" -> "1Pm 2Pbd 2Pko", //睡
    "年" -> "2Pma 2Smb", "重" -> "1Pm 2Ped 1Pk 1Pb", "丿" -> "1Pm",
    "竹" -> "1Pm", "⿱𠂉丶" -> "1Sm", "舌" -> "2Pmi", "𠂉" -> "2Pma",
    "矢" -> "2Sma", "攵" -> "2Pmo", "牛" -> "2Pmb", "失" -> "2Smb 2Pod",
    "𠂒" -> "2Smb", "㐄" -> "2Smb", "气" -> "2Pmy", "氣" -> "2Smy", "毛" -> "2Pmh",
    "禾" -> "2Pmf", "余" -> "2Pod 2Smf", "生" -> "2Pmc", "手" -> "2Pmd",
    "龵" -> "2Smd", "千" -> "2Pme",
    "缶" -> "2Pma 2Ped 2Pzi", "我" -> "2Smd 2Phm")
  private val zmN = HashMap(
    "盥" -> "2Snb 2Pkv 3Plka", //"與" -> "",
    "段" -> "2Tnc 2Pqd 2Pxs", "雋" -> "2Pni 1Pi 1Py 1Pi 1Py",
    "亻" -> "1Pn", "片" -> "2Pnx",
    "川" -> "2Pnd", "⿰丿丨" -> "2Snd", "⿲丿二丨" -> "2Snd", "巟" -> "2Psh 2Pnd",//齊
    "𣶒" -> "2Snd", "臼" -> "2Pnb", "臼丨" -> "2Snb",
    "與" -> "2Snb 2Tid 1Po", "𦥯" -> "2Snb 2Pos 2Pos", "𦥑冖" -> "2Snb",
    "鬼" -> "2Pnj", "⿻白丿" -> "2Snj", "⿱丿囗" -> "2Snj",
    "白" -> "2Pnk", "自" -> "2Pnl", "⿱丿冂" -> "2Snl", "隹" -> "2Pni", "身" -> "2Pnc")
  private val zmO = HashMap(
    "史" -> "1Pj 2Pos", "谷" -> "1Po 2Pod 1Pj",//聲 bxqc
    "八" -> "1Po", "⿱丿丿" -> "2Pod", "人" -> "2Pod",
    "入" -> "2Poda", "乂" -> "2Pos", "㐅" -> "2Pos", "⿻⿱丿丿丶" -> "2Sos",
    "𠚍" -> "2Sos 2Pzi", "彳" -> "2Poi", "行" -> "2Soi",
    "食" -> "2Pox", "飠" -> "2Sox",
  //oi characters - among the first 6000
  //術,徵,衛,衝,街,衡,衍,銜,衙,愆,衢
    "術" -> "2Soi 1Pf 1Ps", "徵" -> "2Soi 2Pll 1Pa 1Pm 1Pb", "衛" -> "2Soi 1Sx 1Pj 2Smb", "衝" -> "2Soi 1Pm 2Ped 1Pk 1Pb",
    "街" -> "2Soi 1Pb 1Pb", "衡" -> "2Soi 1Sr 2Pgd", "衍" -> "2Soi 1Pv", "銜" -> "2Soi 1Pp",
    "衙" -> "2Soi 1Pa 1Sx 1Pj", "愆" -> "2Soi 1Pv 2Pwz", "衢" -> "2Soi 1Pl 1Pl 2Pni"
  )
  private val zmP = HashMap(
    //"爲" -> "",//儰 蟡
    "𠂢" -> "2Spd 2Trh", "㐆" -> "2Spd 2Pxb 1Py",//派
    "金" -> "1Pp", "彡" -> "2Ppd", "斤" -> "2Ppd", "𠂆" -> "2Spd",
    "丘" -> "2Ppda", "豸" -> "2Ppq", "瓜" -> "2Pps", "釆" -> "2Ppf", "采" -> "2Spv 1Pf",
    "爪" -> "2Ppv", "爫" -> "2Spv", "舟" -> "2Ppy")
  private val zmQ = HashMap(
    "册" -> "1Sq 1Sq 1Pa", //珊
    "𠘧" -> "2Pqd", "月" -> "1Pq", "⿵⺆冫" -> "1Pq", //能
    "丹" -> "1Sq 1Ss", "几" -> "2Pqd", "巩" -> "2Pbi 2Sqd",
    "凡" -> "2Pqda", "風" -> "2Pqi", "九" -> "2Pqy", "丸" -> "2Pqya", "殳" -> "2Pqx",
    "犭" -> "2Pqm")
  private val zmR = HashMap(
    "⿴𠂊冫" -> "2Srs", "象" -> "1Pr 1Pj 2Sgq",
    "裊" -> "2Trz 1Ss 2Srh", "梟" -> "2Trz 1Pf",
    "卿" -> "2Srh 2Sxo 1Sy", "兜" -> "2Srh 2Pnk 1Sx 2Prd",
    "旅" -> "2Psy 2Pma 2Srh", "玈" -> "1Ss 1Pz 2Pzs 2Pma 2Srh",
    "祣" -> "2Pws 2Pma 2Srh", "夊" -> "2Srs", //傻
    "印" -> "2Srh 1Pa 1Sy", "⿰⿴𠂊冫②" -> "2Srs", //祭
    "𠂊" -> "1Sr", "⿰③刀" -> "2Srh 1Ps 2Pyd", "免" -> "1Sr 1Pj 2Prd",
    "角" -> "1Sr 1Sl 1Pd", "𩵋" -> "1Sr 2Pgd", "魚" -> "1Sr", "久" -> "1Sr 1Ps",
    "儿" -> "2Prd", "匕" -> "2Prr", "比" -> "2Prr 2Prr", "𠤎" -> "2Srr",
    "勹" -> "2Pry", "包" -> "2Sry", "匃" -> "2Sry",
    "夕" -> "2Prs", "夂" -> "2Srs", "⿴𠂊⺀" -> "2Srs",
    "氏" -> "2Prh", "𠂎" -> "2Srh", "卬" -> "2Srh 1Sy", "𧘇" -> "2Srh",
    "欠" -> "2Pro", "鳥" -> "2Srz", "島" -> "2Srz 2Pll", "烏" -> "2Srza")
  private val zmS = HashMap(
    "主" -> "1Ps 1Pc",
    "乀" -> "1Ps", "褎" -> "1Ss 2Srh 1Pa 2Pmf 2Srh",
    "亦" -> "1Ss 2Snd 1Po", "⿱丶⑤" -> "1Ps 2Pxo", //郎
    "言" -> "1Ps", "亠" -> "1Ss", "文" -> "2Pso", "亡" -> "2Psh", //compare "㐬" -> "a zs nd",
    "立" -> "2Psu", "辛" -> "2Pse", "方" -> "2Psy", "永" -> "2Psk", "龍" -> "2Psi",
  "丶" -> "1Ps")
  private val zmT = HashMap(
    "廌" -> "2Stx 1Pa 1Pz 2Suo", //etzu
    "疒" -> "1Pt", "病" -> "1St", "冫" -> "2Ptd", "⺀" -> "2Std",
    "北" -> "2Sti 2Prr", "广" -> "2Ptg", "廣" -> "2Stg", "鹿" -> "2Ptx",
    "⿸广⿻コ⿰丨丨" -> "2Stx")
  private val zmU = HashMap(
    //chars that didnt have a shape breakdown:
    "羲" -> "2Suc 2Pmf 1Pa 1Pz 2Phm", "儰" -> "1Pn 1Pe 2Sud 1Py 2Suo",//犧
    "為" -> "2Sud 1Py 2Suo", "蟡" -> "1P1 2Sud 1Py 2Suo", "撝" -> "1Pd 2Sud 1Py 2Suo",
    "噅" -> "1Pj 2Sud 1Py 2Suo", "𤾡" -> "2Pnk 2Sud 1Py 2Suo", //"蘤"
    "辨" -> "2Pse 2Sud 2Pse", "寪" -> "2Pwd 2Suo 1Py 2Suo", "鄬" -> "2Suo 1Py 2Suo 1Py"
    ,"关" -> "2Pua 2Pgd" //送
    , "忄" -> "1Pu", "㣺" -> "1Su", "米" -> "2Puf", "丷" -> "1Sud",
    "②又" -> "1Sud 2Pxs"//𡉢 帰
    , "②帚" -> "1Sud 2Pxb 2Pww 2Pli", "丷下" -> "2Pua 2Pid", "䒑" -> "2Pua"
    , "乎" -> "1Pm 2Sua", "⿻干丷" -> "1Ps 2Sua" //平
    , "火" -> "2Puo", "灬" -> "2Suo"//灰 𤇴
    , "半" -> "2Pub", "𢆉" -> "2Sub", "龹" -> "2Sub"//伴 㚔 堘 (2 missing)
    , "⿱䒑⿻二丨" -> "2Puc", "𦍌" -> "2Suc"//𡗗 差 𦍼 𢜗 𦍌 羊  𩱁
  )
  private val zmV = HashMap(
    "州" -> "2Pvd 2Pnd",
    "氵" -> "1Pv", "⿻一氺" -> "1Sd 1Sv", "⿱丷八" -> "1Sv"//𢯼 求 剑 𠦂
     , "佥" -> "2Pod 2Pbd 2Pvd", "兆" -> "1Sv 2Prd"
  )
  private val zmW = HashMap(
    "之" -> "1Pw", "辶" -> "1Sw", "令" -> "2Pod 1Sw", "礻" -> "2Pws",
    "宀" -> "2Pwd", "定" -> "2Swd", "冖" -> "2Pww"//之 辶 令 宀 冖 定
    , "穴" -> "2Pwo", "冘" -> "2Pww 2Prd"
  , "衤" -> "2Pws", "衤" -> "2Pwt"
    , "戶" -> "2Spd 1Sx" //"戶" -> "2Pwm" //𫉚 戶
   , "心" -> "2Pwz"//心 忠
   , "黽" -> "2Pwx", "龜" -> "1Pr 2Swx" //澠 龜
  )
  private val zmX = HashMap(
    "臦" -> "1Sx 1Pi 1Pa 1Pz 1Pi 1Sh",
    "丑" -> "1Sx 2Ped", "𤴔" -> "1Px 2Pii", "鵖" -> "2Pnk 2Prr 2Srz", //羞 疏
    "肅" -> "2Sxb 2Snd", "尺" -> "1Pxm 1Ps", "𡬠" -> "2Sxo 2Pds",
    "彐" -> "2Pxb", "⿻コ一" -> "2Pxb", //寢
    "承" -> "1Px 2Pkv 2Pcd", "⿻⿻コ一④" -> "2Pxk", //尋 兼
    "隶" -> "2Sxb 2Skv", "尹" -> "3Pxma", "疋" -> "1Px 2Sii", "⿻⿻コ一亅" -> "2Sxb", //爭 君
    "叚" -> "1Sx 2Pbd 1Sx 2Pxs", "既" -> "2Sxo 1Ph 2Prd",
    "即" -> "2Sxo 1Sy", "⿳𦥑一" -> "2Snb", "龴" -> "2Sxs", "乛" -> "1Px", //興
    "廄" -> "2Ptg 2Sxo 2Pqx",
    "皮" -> "2Pxi", "𠃜" -> "2Sxm", "𫝀" -> "1Sx", //韋 xjmb 衛 oijm ??? - ser ud til at det sidste element er glemt
    "癶" -> "2Sxs", "尸" -> "2Pxm",
    "𠃍" -> "1Px", "㇇" -> "1Px", "肀" -> "2Sxb", "ユ" -> "1Sx",
    "艮" -> "2Pxo", "⿱𦘒一" -> "2Sxb",
    "幺" -> "1Pz 2Pzs",
    "門" -> "2Pxd",//⿵門口
    "又" -> "2Pxs"
  )

  //衛 𫝀

  private val zmY = HashMap(
    "⿰丿𠃌" -> "2Sym", "刃" -> "2Pyd 1Ps", //別 認
    "乜" -> "1Py 1Pz", "⿱勹又" -> "2Sym 2Pxs", "⿻刀二" -> "1Py 2Sbi", //沒vyxs 那yby "1Pyd 2Sbi"
    "卍" -> "1Py 1Pi 1Pa 1Pi", "孑" -> "1Ty 1Ta", "孓" -> "1Ty 1Ps",
    "⿲刀丫③" -> "1Py 2Pud 1Pi 2Srh", "㔾" -> "2Syyb", //齊 犯
    "民" -> "3Syyb 2Shd", "及" -> "1Ty 1Pm 1Ps", "飛" -> "3Syda 1Sv 3Syda 1Sv 1Pm 2Snd",
    "𠃌" -> "1Py", "㠯" -> "1Sy", "廴" -> "1Sy", "卩" -> "1Py",
    "阝" -> "1Py", "了" -> "1Py", "刀" -> "2Pyd", "乙" -> "2Pyda", "𠄎" -> "1Py",
    "⺄" -> "2Syda", "也" -> "2Pyi", "巴" -> "2Pyia", "子" -> "2Pya",
    "力" -> "2Pym", "⿻丿𠃌" -> "2Sym", "己" -> "2Pyy", "已" -> "2Pyya",
    "巳" -> "2Pyyb", "弓" -> "2Pyz", "𢎨" -> "2Syz", "习" -> "2Pyt")
  private val zmZ = HashMap(
    "焉" -> "1Pa 2Pii 1Pa 1Pz 2Suo",
    "巤" -> "2Pzd 2Pjd 2Pos 2Szd", //獵
    "𠃑" -> "1Pz", "㇉" -> "1Pz", "𠃊" -> "1Pz", //,"𢆸" -> "1Pz 2Pzs 1Pz 2Pzs 1Pz",//斷
    "乡" -> "1Pz 2Szm", "夨" -> "1Pz 1Pa 1Ps",//奊
    "糹" -> "1Sz", "糸" -> "1Sz", "毌" -> "2Szy", "丱" -> "2Szi",
    "乚" -> "1Pz",
    "以" -> "1Pz 1Ps 2Pod" //denne har ikke et ids opslag
  //z single stroke - 𣭖
  //mangle 1Pz. maaske fordi den kun findes som simplificeret
  //𠀔 𠫔 𣦶 𠀅 𢻽 𣨒 㙲
  , "䜌" -> "2Pzs" //𦣏
  , "巛" -> "2Pzd"
  , "巜" -> "2Szd"
  , "鼠" -> "2Pnb 2Szd" //𡿬 𤰕 𣜌 鼠 𤢪
  //, "𤢪" -> skal tilfoejes hvis noedvendigt
  , "厶" -> "2Pzs"
  //厶 勾 𠇇
  , "女" -> "2Pzm"
  , "互" -> "2Pbd 2Szm"
  , "彑" -> "2Szm"
  , "𠂈" -> "2Szm"
  //𠯆 𠯞 彑
  , "母" -> "2Pzy"
  , "毋" -> "2Szy" //der mangler en 2Szy form
  //每 毒 𩬍 毋
  , "爿" -> "2Szi"
  , "凵" -> "2Pzi"
  , "丩" -> "2Szi"
  , "屮" -> "2Szi"
  , "艸" -> "2Szi"
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
