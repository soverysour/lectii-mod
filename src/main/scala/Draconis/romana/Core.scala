package Draconis.romana

import org.apache.commons.io.FileUtils

object Core {

  type File = java.io.File

  private[this] def readF(path: String): IndexedSeq[String] = FileUtils.
    readFileToString(new File(path),"UTF-8").split("\n").toIndexedSeq

  private[this] def writeF(path: String, data: String, app: Boolean): Unit = FileUtils.
    write(new File(path), data, "UTF-8", app)

  def main(args: Array[String]): Unit = {
    Holder.loadUsers(readF(System.getProperty("user.home") + "/Draconis/users.txt"))
    Frame.initial
  }

  def initModule(s: String): Unit = {
    Holder.setModule(s)

    readF(System.getProperty("user.home") + "/Draconis/" + s + "/settings.txt").foreach(x => {
      val Array(s1, s2, s3) = x.split("[=]")
      if (s1 == "1") Holder.setSettings(s2 -> s3)
      else {
        val path = System.getProperty("user.home") + "/Draconis/" + s
        val (one, two) = (s3.split("[,]")(0), s3.split("[,]")(1).toInt)
        if (s1 == "2")
          Holder.addLectura(readF(path + "/material/" + s2), one, two)
        else if (s1 == "3")
          Holder.addTest(readF(path + "/test/" + s2), one, two)
        else if (s1 == "4")
          Holder.addGallery(one, two)
      }
    })
  }

  def register(us: String, pa: String, nu: String, pr: String, sc: String,
    opt: String, isElev: Boolean): Unit = {
    var sp = if (isElev) "e" else "p"
    val link = s"$us,$pa,$nu,$pr,$sc,$opt,$sp"

    Holder.loadUsers(IndexedSeq(link))
    writeF(System.getProperty("user.home") + "/Draconis/users.txt", link + "\n", true)
  }

  def getModules: IndexedSeq[String] =
    readF(System.getProperty("user.home") + "/Draconis/modules.txt")

  def evaluate(newSp: List[(String, String)], newCh: List[(String, List[(String, Boolean)])],
    leftRi: List[(String, String)], id: String): Unit = {
    val path = System.getProperty("user.home") + "/Draconis/" + Holder.getModule + "/"
    var brute = ""
    var score = ""

    val ultCh = newCh.map( x => {
      x._1 -> x._2.map( y => {
        if ( y._2 ) y._1 + "#"
        else y._1
      })
    })

    Holder.getExactTest(id) match {
      case Some(x) =>
        for ( alfa <- x.problems ){
          if ( alfa.tip == "C##E" ){
            for {
              beta <- alfa.exercitii
              ss <- newSp
              if ss._1 == beta._1
              if ss._2 != ""
            } if ( beta._2.split("[,]").map(_.toLowerCase.trim ) contains ss._2.toLowerCase.trim )
            brute = brute + "V:" + beta._1 + "##" + ss._2.toLowerCase.trim + "\n"
            else brute = brute + "P:" + beta._1 + "##" + ss._2.toLowerCase.trim + "\n"
          }
          else if ( alfa.tip == "D##D" ){
            for {
              beta <- alfa.exercitii
              ss <- leftRi
              if beta._1 == ss._1
            } if ( beta._2 == ss._2 ) brute = brute + "V:" + beta._1 + "##" + ss._2 + "\n"
            else brute = brute + "P:" + beta._1 + "##" + ss._2 + "\n"
          }
          else if ( alfa.tip == "C##V" ){
            for {
              beta <- alfa.exercitii
              ss <- ultCh
              if beta._1 == ss._1
            } for {
              gamma <- ss._2
            } if ( beta._2.split("[,]") contains gamma )
            brute = brute + "V:" + beta._1 + "##" + gamma + "\n"
            else brute = brute + "P:" + beta._1 + "##" + gamma + "\n"
          }
        }
      case None => {}
    }

    var xx = 0
    readF(path + "dictionary.txt").foreach( x => if ( x contains id ) xx += 1 )

    val u =  Holder.getUser.username

    writeF(path + "dictionary.txt", s"T:${id}:${xx}:${score}:${u}\n", true)
    writeF(path + "/progress/" + s"${id}-${xx}-${u}", brute + "\n\n" + score, false)
  }

}
