package Draconis.romana

object Defaults {

  object Names {
    val settingName: String = "S"
    val materialName: String = "M"
    val testName: String = "T"
    val galleryName: String = "G"

    val problemEnd = "##"
    val testSeparator = "##"
    val testEntrySeparator = "#"
    val variantIdentifier = "#"

    val dragDrop: String = "D##D"
    val chooseVariant: String = "C##V"
    val completeSpace: String = "C##E"

    val good = "V"
    val bad = "P"

    val elevMark = "e"
    val professorMark = "p"

  }

  object Paths {
    val homePath: String = s"${System.getProperty("user.home")}/Draconis/"
    val usersPath: String = s"${homePath}users.txt"
    val modulePath: String = s"${homePath}modules.txt"

    def homeModulePath: String = s"${homePath}${Holder.getModule}/"
    def settingsPath: String = s"${homeModulePath}settings.txt"
    def dictionaryPath: String = s"${homeModulePath}dictionary.txt"
    def materialPath: String = s"${homeModulePath}material/"
    def testPath: String = s"${homeModulePath}test/"
    def galleryPath: String = s"${homeModulePath}gallery/"
    def progressPath: String = s"${homeModulePath}progress/"
  }

  object Misc {
    val encoding: String = "UTF-8"

    def m_prettify(s: String): String = s.toLowerCase.trim
    def m_verifyAmount(sum: Double, n: Double): String = if ( (sum/n).toString == "NaN" ) "0"
      else (sum/n).toString
  }

  object ProcessAccountsSettings {
    private[this] val settingSep = "="
    private[this] val nameLevelSep = ","
    private[this] val accountSep = ","

    def s_splitData(s: String): Array[String] = s.split(s"[${settingSep}]")
    def s_isProperEntry(s: String): Boolean = s.contains(settingSep)
    def s_getName(s: String): String = s.split(s"[${nameLevelSep}]")(0)
    def s_getLevel(s: String): Int = s.split(s"[${nameLevelSep}]")(1).toInt

    def a_isProperEntry(s: String): Boolean = s.contains(accountSep)
    def a_splitData(s: String): Array[String] = s.split(s"[${accountSep}]")
    def a_formatData(s: List[String]): String = {
      var alfa = ""
      for ( x <- s ) alfa = s"${alfa}${accountSep}${x}"
      alfa.drop(1)
    }
  }

  object ProcessDictionaryEntries {
    def d_isProperEntry(s: String): Boolean = s.contains("#")
    def d_getType(s: String): String = s.take(1)
    def d_getId(s: String): String = s.split(testEntrySeparator)(1)
    def d_getDiscr(s: String): String = s.split(testEntrySeparator)(2)
    def d_getUser(s: String): String = s.split(testEntrySeparator)(3)
    def d_getResult(s: String): String = s.split(testEntrySeparator)(4)
    def d_getScore(s: String): Double = s.split(testEntrySeparator)(4).split("[/]")(0).toDouble
    def d_getTotal(s: String): Double = s.split(testEntrySeparator)(4).split("[/]")(1).toDouble

    def d_formatScore(i: Double, j: Double): String = s"${i}/${j}"
    def d_formatEntry(id: String, discr: Int, user: String, score: String) : String = {
      s"T#${id}#${discr}#${user}#${score}\n"
    }
    def d_formatPath(id: String, discr: Int, username: String): String = {
      s"${Paths.progressPath}T--${id}--${discr}--${username}"
    }
  }

  object ProcessTestEntries {
    def t_getType(s: String): String = s.take(4)
    def t_getStatus(s: String): String = s.drop(5).take(2)
    def t_getName(s: String): String = s.drop(7).split(Names.testSeparator)(0)
    def t_getSolution(s: String): String = s.drop(7).split(Names.testSeparator)(1)

    def t_format(info: String, ask: String, ans: String, good: Boolean, kind: String): String = if (good)
      s"${info}${kind}:${Names.good}:${ask}${Names.testSeparator}${ans}\n"
    else s"${info}${kind}:${Names.bad}:${ask}${Names.testSeparator}${ans}\n"
  }
  object ProcessRawTest {
    private[this] val testAnsSep = ","

    def r_splitAns(s: String): Array[String] = s.split(s"[${testAnsSep}]")
    def r_isHeader(s: String): Boolean = {
      if (s.startsWith(Names.chooseVariant) ||
          s.startsWith(Names.dragDrop) ||
          s.startsWith(Names.completeSpace))
        true
      else false
    }
  }

}
