package Draconis.romana

import java.io.File
import org.apache.commons.io.FileUtils

object Core {

  private[this] def readF(path: String): IndexedSeq[String] = FileUtils.
    readFileToString(new File(path),"UTF-8").split("\n").toIndexedSeq

  private[this] def writeF(path: String, data: String, app: Boolean): Unit = FileUtils.
    write(new File(path), data, "UTF-8", app)

  def main(args: Array[String]): Unit = {
    Holder.loadUsers(readF(System.getProperty("user.home")+"/Draconis/users.txt"))
    Frame.initial
  }

  def initModule(s: String): Unit = {
    Holder.setModule(s)

    readF(System.getProperty("user.home") + "/Draconis/" + s + "/settings.txt").foreach(x => {
      val Array(s1, s2, s3) = x.split("[=]")
      if (s1 == "1") Holder.setSettings(s2 -> s3)
      else {
        val path = System.getProperty("user.home") + "/Draconis/" + Holder.getModule
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

    var toSend = ""

  }
}
