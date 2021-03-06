package Draconis.romana

object Defaults {

  object Frame {
    val lId: String = "XDvCSSCvDX7Dra8"

    val l_title: String = "Login"
    val l_usernameLabel: String = "Utilizator"
    val l_passwordLabel: String = "Parola"
    val l_loginButton: String = "Login"
    val l_registerButton: String = "Inregistrare"
    val l_noUserMessage: String = "Utilizatorul nu exista."
    val l_badPasswordMessage: String = "Parola incorecta."
    val l_invalidData: String = "Eroare de autentificare"

    val r_title: String =  "Inregistrare"
    val r_usernameLabel: String = "Nume Utilizator"
    val r_passwordLabel: String = "Parola"
    val r_surnameLabel: String = "Nume"
    val r_nameLabel: String = "Prenume"
    val r_schoolLabel: String = "Scoala"
    val r_optionalLabelStudent: String = "Clasa"
    val r_optionalLabelProfessor: String = "Disciplina"
    val r_radioLabelStudent: String = "Elev"
    val r_radioLabelProfessor: String = "Profesor"
    val r_creationButtonLabel: String = "Creare"
    val r_quitButtonLabel: String = "Inapoi"
    val r_unallowedChars: String = "[#]"
    val r_invalidTokenMessage: String = "Artefact invalid."
    val r_invalidData: String = "Eroare date introduse."
    val r_userConflictMessage: String = "Utilizatorul deja exista."
    val r_invalidInputMessage: String = "Eroare date"

    val s_materialTab: String = "Materiale"
    val s_testTab: String = "Teste"
    val s_profileTab: String = "Profil"
    val s_profileUser: String = "Utilizator"
    val s_profileSurname: String = "Nume"
    val s_profileName: String = "Prenume"
    val s_profileSchool: String = "Scoala"
    val s_profileClass: String = "Clasa"
    val s_pastTests: String = "Teste date"
    val s_changeModule: String = "Schimba lectia"

    val p_title: String = "Admin"

    val t_finishButton: String = "Am terminat"
    val t_closeDialogMessage: String = "Esti sigur ca progresul pana acum e definitiv? Decizia nu se poate revoca."
    val t_closeDialogTitle: String = "Atentie"
    val t_closeDialogEntries: List[String] = List("Da", "Nu")

    val m_title: String = "Alege lectia"
    val if_title: String = "Teste date"
    val rf_title: String = "Tipuri de teste"
    val tt_good: String = "CORECT:"
    val tt_bad: String = "INCORECT:"

    val pf_title: String = "Clipuri audio"
    val pf_next: String = "Urmatorul"
    val pf_previous: String = "Inapoi"
    val pf_toggle: String = "Opreste/Porneste"
    val pf_noSongs: String = "Lista goala"
  }

  object Names {
    val id_username: Int = 0
    val id_password: Int = 1
    val id_surname: Int = 2
    val id_name: Int = 3
    val id_school: Int = 4
    val id_optional: Int = 5

    val settingName: String = "S"
    val materialName: String = "M"
    val testName: String = "T"
    val musicName: String = "X"

    val nonExisting: String = "null"

    val defaultSeparator: String = "##"
    val powerSeparator: String = s"${defaultSeparator}${defaultSeparator}"
    val defaultIdentifier: String = "@@"

    val dragDrop: String = "DD"
    val chooseVariant: String = "CV"
    val completeSpace: String = "CE"

    val good: String = "V"
    val bad: String = "P"

    val studentMark = "student"
    val professorMark = "professor"
  }

  object Paths {
    private[this] val delimiter: String = {
      if (System.getProperty("os.name").toLowerCase.contains("windows")) "\\"
      else "/"
    }

    val homePath: String = s"${System.getProperty("user.home")}${delimiter}lectii-mod${delimiter}"
    val usersPath: String = s"${homePath}users.txt"
    val modulePath: String = s"${homePath}modules.txt"
    val typePath: String = s"${homePath}channel.txt"

    def homeModulePath: String = s"${homePath}${Holder.getModule}${delimiter}"
    def settingsPath: String = s"${homeModulePath}settings.txt"
    def dictionaryPath: String = s"${homeModulePath}dictionary.txt"
    def materialPath: String = s"${homeModulePath}material${delimiter}"
    def testPath: String = s"${homeModulePath}test${delimiter}"
    def progressPath: String = s"${homeModulePath}progress${delimiter}"
  }

  object Misc {
    val encoding: String = "UTF-8"

    def m_isVerified(s: String): Boolean = s.drop(3).startsWith(Names.good)
    def m_prettify(s: String): String = s.toLowerCase.trim
    def m_verifyAmount(sum: Double, n: Double): String = if ( (sum/n).toString == "NaN" ) "0" else (sum/n).toString
    def m_truncate(s: String): String = if ( s.split("[.]").size > 1 ){
      val b = s.split("[.]")(1)
      s.split("[.]")(0) +"."+ b.take(2)
    } else s
  }

  object ProcessAccountsSettings {
    def s_splitData(s: String): Array[String] = s.split(Names.powerSeparator)
    def s_isProperEntry(s: String): Boolean = s.contains(Names.powerSeparator)
    def s_getName(s: String): String = s.split(Names.defaultSeparator)(0)
    def s_getLevel(s: String): Int = s.split(Names.defaultSeparator)(1).toInt

    def a_isProperEntry(s: String): Boolean = s.contains(Names.defaultSeparator)
    def a_splitData(s: String): Array[String] = s.split(Names.defaultSeparator)
    def a_formatData(s: List[String]): String = (for ( x <- s ) yield s"${Names.defaultSeparator}${x}").mkString.drop(Names.defaultSeparator.size)
  }

  object ProcessDictionaryEntries {
    def d_isProperEntry(s: String): Boolean = s.contains(Names.defaultSeparator)
    def d_getType(s: String): String = s.take(1)
    def d_getId(s: String): String = s.split(Names.defaultSeparator)(1)
    def d_getDiscr(s: String): String = s.split(Names.defaultSeparator)(2)
    def d_getUser(s: String): String = s.split(Names.defaultSeparator)(3)
    def d_getResult(s: String): String = s.split(Names.defaultSeparator)(4)
    def d_getScore(s: String): Double = s.split(Names.defaultSeparator)(4).split("[/]")(0).toDouble
    def d_getTotal(s: String): Double = s.split(Names.defaultSeparator)(4).split("[/]")(1).toDouble

    def d_formatScore(i: Double, j: Double): String = s"${i}/${j}"
    def d_formatEntry(id: String, discr: Int, score: String) : String = s"${Names.testName}${Names.defaultSeparator}${id}${Names.defaultSeparator}${discr}${Names.defaultSeparator}${Holder.getUser.username}${Names.defaultSeparator}${score}\n"
    def d_formatPath(id: String, discr: Int): String = s"${Paths.progressPath}${Names.testName}--${id}--${discr}--${Holder.getUser.username}"
  }

  object ProcessTestEntries {
    def t_getType(s: String): String = s.take(2)
    def t_getStatus(s: String): String = s.drop(3).take(1)
    def t_getName(s: String): String = s.drop(5).split(Names.powerSeparator)(0)
    def t_getSolution(s: String): String = s.drop(5).split(Names.powerSeparator)(1)
    def t_hasSolution(s: String): Boolean = s.drop(5).split(Names.powerSeparator).size > 1
    def t_splitPro(s: String): Array[String] = s.split(Names.powerSeparator)
    def t_splitAns(s: String): Array[String] = s.split(Names.defaultSeparator)
    def t_isSelected(s: String): Boolean = s.endsWith(Names.defaultIdentifier)
    def t_getOption(s: String): String = s.dropRight(Names.defaultIdentifier.size)

    def t_isHeader(s: String): Boolean = {
      if (s.trim == Names.chooseVariant || s.trim == Names.dragDrop || s.trim == Names.completeSpace) true
      else false
    }

    def t_format(info: String, ask: String, ans: String, good: Boolean, kind: String): String = if (good)
      s"${info}${kind}:${Names.good}:${ask}${Names.powerSeparator}${ans}\n"
    else s"${info}${kind}:${Names.bad}:${ask}${Names.powerSeparator}${ans}\n"
  }

}
