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

    for (eachLineElem: String <- source.getLines()) {
      if (eachLineElem != null && eachLineElem.size > 1) {
        tempLines += eachLineElem.split("\\s+").toList
      }
    }
    //tempLines.toList.foreach(x => idsLinesMap + (x(1) -> x(2)))
    tzaiLines = tempLines.toList
    source.close()
    
    //tzaiSimpelList
    tzaiSimpelList = tempLines.toList.map(each => each(0))
    
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



  private val zmA = HashMap("一" -> "1Pa", "丁" -> "2Pai",
      "瓦" -> "1Pa 1Pz 1Py 1Pz")
  private val zmB = HashMap("土" -> "1Pb", "士" -> "1Sb", "二" -> "2Pbd", "示" -> "2Pbk",
    "走" -> "2Pbo", "耂" -> "2Pbm", "者" -> "2Sbm", "工" -> "2Pbi","亞" -> "2Pbz")
  private val zmC = HashMap("王" -> "1Pc", "三" -> "2Pcd", "玉" -> "2Pcs", "耳" -> "2Pce", "馬" -> "2Pcu",
    "髟" -> "2Pch", "長" -> "2Sch", "镸" -> "2Sch", "丰" -> "2Pci", "龶" -> "2Sci",
    "豐" -> "2Sci", "耒" -> "2Pck", "鬥" -> "2Pcc", "𡗗" -> "2Pco", "春" -> "2Sco")
  private val zmD = HashMap("扌" -> "1Pd", "寸" -> "2Pds")

  private val zmE = HashMap("卌" -> "1Te", "艹" -> "1Pe", "廾" -> "1Se", "十" -> "2Ped", "革" -> "2Pee",
    "廿" -> "2Pea", "龷" -> "2Sea", "卌一" -> "2Sea", "帶" -> "2Sea 2Pww 2Pll",
    "甘" -> "2Peb", "其" -> "2Pec", "栽" -> "2Peh", "𢦏" -> "2Seh")
  private val zmF = HashMap("木" -> "1Pf", "酉" -> "2Pfd", "覀" -> "2Pfj", "西" -> "2Sfj",
    "車" -> "2Pfk", "專" -> "2Sfk 2Sds", "甫" -> "2Pfb", "雨" -> "2Pfv")
  private val zmG = HashMap("石" -> "1Pg", "丆" -> "1Sg", "厂" -> "2Pgg", "大" -> "2Pgd",
    "𠂇" -> "2Sgd", "辰" -> "2Pgh", "尨" -> "2Sgm", "不" -> "2Pgi", "頁" -> "2Sgo",
    "而" -> "2Pgl", "豕" -> "2Pgq", "𧰨" -> "2Sgq",
    "在" -> "2Sgd 1Pi 1Pb")
  private val zmH = HashMap("匚" -> "1Ph", "臣" -> "1Sh", "一⿰𠄌⿺乀丿" -> "1Sh", "七" -> "2Phd",
    "巠" -> "2Phd 2Pbi", "弋" -> "2Phs", "戈" -> "2Phm", "戊" -> "2Shm",
    "牙" -> "2Phi", "至" -> "2Phb") //𠥫
  private val zmI = HashMap("虫" -> "1Pi", "卜" -> "2Pid", "乍" -> "2Pma 2Sid",
    "虍" -> "2Pih", "虎" -> "2Sih", "止" -> "2Pii", "龰" -> "2Sii", "齒" -> "2Sio")
  private val zmJ = HashMap("口" -> "1Pj", "囗" -> "2Pjd", "因" -> "2Sjd", "足" -> "2Pji", "𧾷" -> "2Sji")
  private val zmK = HashMap("日" -> "1Pk", "曰" -> "1Sk", "⿻口一" -> "1Sk", "刂" -> "2Pkd", "业" -> "2Pku",
    "業" -> "2Sku", "婁" -> "2Pkj 2Pzm", "非" -> "2Pkc", "小" -> "2Pko", "⺌" -> "2Sko",
    "𣥂" -> "2Sko", "水" -> "2Pkv", "氺" -> "2Skv", "㡀" -> "2Skv 2Pld", "眔" -> "2Plk 2Skv",
    "田" -> "2Pki", "由" -> "2Pkia", "甲" -> "2Pkib", "申" -> "2Pkic")
  private val zmL = HashMap("目" -> "1Pl", "冂" -> "2Pld", "同" -> "2Sld", "𠔼" -> "2Sld",
    "冋" -> "2Sld", "冏" -> "2Sld", "⿵冂𢆉" -> "2Sld", "用" -> "2Sld",
    "甬" -> "2Sxs 2Sld", "角" -> "1Sr 2Sld", "𠕁" -> "2Sld", "岡" -> "2Sld",
    "巾" -> "2Pli", "山" -> "2Pll", "罒" -> "2Plk", "四" -> "2Slk", "曾" -> "2Pud 2Slk 1Pk",
    "會" -> "1Pod 1Ps 2Slk 1Pk", "皿" -> "2Plka", "且" -> "2Plc", "⿴且一" -> "2Slc",
    "貝" -> "2Slo", "咼" -> "2Plj", "見" -> "2Slr", "骨" -> "2Plw")
  private val zmM = HashMap(
    //𣏈 m shape - single stroke
    "竹" -> "1Pm", "⿱𠂉丶" -> "1Sm", "舌" -> "2Pmi", "𠂉" -> "2Pma",
    "矢" -> "2Sma", "攵" -> "2Pmo", "牛" -> "2Pmb", "失" -> "2Smb 2Pod",
    "𠂒" -> "2Smb", "㐄" -> "2Smb", "气" -> "2Pmy", "氣" -> "2Smy", "毛" -> "2Pmh",
    "禾" -> "2Pmf", "余" -> "2Pod 2Smf", "生" -> "2Pmc", "手" -> "2Pmd",
    "龵" -> "2Smd", "千" -> "2Pme",
    "缶" -> "2Pma 2Ped 2Pzi", "我" -> "2Smd 2Phm")
  private val zmN = HashMap("亻" -> "1Pn", "片" -> "2Pnx", "川" -> "2Pnd", "⿰丿丨" -> "2Snd",
    "𣶒" -> "2Snd", "㐬" -> "2Psh 2Snd", "臼" -> "2Pnb", "臼丨" -> "2Snb",
    "與" -> "2Snb 1Po", "𦥯" -> "2Snb 2Pos 2Pos", "𦥑冖" -> "2Snb",
    "鬼" -> "2Pnj", "⿻白丿" -> "2Snj", "⿱丿囗" -> "2Snj",
    "白" -> "2Pnk", "自" -> "2Pnl", "⿱丿冂" -> "2Snl", "隹" -> "2Pni", "身" -> "2Pnc")
  private val zmO = HashMap("八" -> "1Po", "⿱丿丿" -> "2Pod", "人" -> "2Pod",
    "入" -> "2Poda", "乂" -> "2Pos", "㐅" -> "2Pos", "⿻⿱丿丿丶" -> "2Sos",
    "𠚍" -> "2Sos 2Pzi", "彳" -> "2Poi", "行" -> "2Soi",
    "食" -> "2Pox", "飠" -> "2Sox")
  private val zmP = HashMap("金" -> "1Pp", "彡" -> "2Ppd", "斤" -> "2Ppd", "𠂆" -> "2Spd",
    "丘" -> "2Ppda", "豸" -> "2Ppq", "瓜" -> "2Pps", "釆" -> "2Pps", "采" -> "2Pps",
    "爪" -> "2Ppv", "爫" -> "2Spv", "舟" -> "2Ppy")
  private val zmQ = HashMap("月" -> "1Pq", "丹" -> "1Sq 1Ss", "几" -> "2Pqd", "巩" -> "2Pbi 2Sqd",
    "凡" -> "2Pqda", "風" -> "2Pqi", "九" -> "2Pqy", "丸" -> "2Pqya", "殳" -> "2Pqx",
    "犭" -> "2Pqm")
  private val zmR = HashMap("𠂊" -> "1Sr", "角" -> "1Sr 1Sl 1Pd", "𩵋" -> "1Sr 2Pgd", "魚" -> "1Sr",
    "儿" -> "2Prd", "匕" -> "2Prr", "比" -> "2Prr 2Prr", "𠤎" -> "2Srr",
    "勹" -> "2Pry", "包" -> "2Sry", "匃" -> "2Sry",
    "夕" -> "2Prs", "夂" -> "2Srs", "⿴𠂊⺀" -> "2Srs",
    "氏" -> "2Prh", "𠂎" -> "2Srh", "卬" -> "2Srh 1Sy", "𧘇" -> "2Srh",
    "欠" -> "2Pro", "鳥" -> "2Srz", "島" -> "2Srz 2Pll", "烏" -> "2Srza")
  private val zmS = HashMap("言" -> "1Ps", "亠" -> "1Ss", "文" -> "2Pso", "亡" -> "2Psh",
    "立" -> "2Psu", "辛" -> "2Pse", "方" -> "2Psy", "永" -> "2Psk", "龍" -> "2Psi",
  "丶" -> "1Ps")
  private val zmT = HashMap("疒" -> "1Pt", "病" -> "1St", "冫" -> "2Ptd", "⺀" -> "2Std",
    "北" -> "2Sti 2Prr", "广" -> "2Ptg", "廣" -> "2Stg", "鹿" -> "2Ptx",
    "⿸广⿻コ⿰丨丨" -> "2Stx")
  private val zmU = HashMap("忄" -> "1Pu", "㣺" -> "1Su", "米" -> "2Puf", "丷" -> "1Sud",
    "②又" -> "1Sud 2Pxs"//𡉢 帰
    , "②帚" -> "1Sud 2Pxb 2Pww 2Pli", "丷下" -> "2Pua 2Pid", "䒑" -> "2Pua"
    , "乎" -> "1Pm 2Sua", "⿻干丷" -> "1Ps 2Sua" //平
    , "火" -> "2Puo", "灬" -> "2Suo"//灰 𤇴
    , "半" -> "2Pub", "𢆉" -> "2Sub", "龹" -> "2Sub"//伴 㚔 堘 (2 missing)
    , "⿱䒑⿻二丨" -> "2Puc", "𡗗" -> "2Suc", "𦍌" -> "2Suc"//𡗗 差 𦍼 𢜗 𦍌 羊  𩱁
  )
  private val zmV = HashMap(
    "氵" -> "1Pv", "⿻一氺" -> "1Sd 1Sv", "⿱丷八" -> "1Sv"//𢯼 求 剑 𠦂
     , "佥" -> "2Pod 2Pbd 2Pvd"
  )
  private val zmW = HashMap(
    "之" -> "1Pw", "辶" -> "1Sw", "令" -> "2Pod 1Sw",
    "宀" -> "2Pwd", "定" -> "2Swd", "冖" -> "2Pww"//之 辶 令 宀 冖 定
    , "穴" -> "2Pwo"//穴
  , "衤" -> "2Pws", "衤" -> "2Pwt"
    , "戶" -> "2Pwm" //𫉚 戶
   , "心" -> "2Pwz"//心 忠
   , "黽" -> "2Pwx", "龜" -> "1Pr 2Swx" //澠 龜
  )
  private val zmX = HashMap(

    //z single stroke - 𣭖
    "糸" -> "1Sz"
    //mangle 1Pz. maaske fordi den kun findes som simplificeret
    //𠀔 𠫔 𣦶 𠀅 𢻽 𣨒 㙲
    , "䜌" -> "2Pzs"  //𦣏
    ,"巛" -> "2Pzd", "巜" -> "2Szd", "鼠" -> "2Pnb 2Szd"//𡿬 𤰕 𣜌 鼠 𤢪
    //, "𤢪" -> skal tilfoejes hvis noedvendigt
    , "厶" -> "2Pzs"
    //厶 勾 𠇇
    , "女" -> "2Pzm", "互" -> "2Pbd 2Szm", "彑" -> "2Szm", "𠂈" -> "2Szm"
    //𠯆 𠯞 彑
     , "母" -> "2Pzy", "毋" -> "2Szy" //der mangler en 2Szy form
    //每 毒 𩬍 毋
     , "爿" -> "2Szi", "凵" -> "2Pzi", "丩" -> "2Szi", "屮" -> "2Szi", "艸" -> "2Szi"
    //𤖀 𠁫 𠇿 𡴅 𦱶 𡴳 -venstre side af Ding mangler
  )
  private val zmY = HashMap("阝" -> "1Py", "了" -> "1Py", "刀" -> "2Pyd", "乙" -> "2Pyda",
    "⺄" -> "2Syda", "也" -> "2Pyi", "巴" -> "2Pyia", "子" -> "2Pya",
    "力" -> "2Pym", "⿻丿𠃌" -> "2Sym", "己" -> "2Pyy", "已" -> "2Pyya",
    "巳" -> "2Pyyb", "弓" -> "2Pyz", "𢎨" -> "2Syz", "习" -> "2Pyt")
  private val zmZ = HashMap()

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
