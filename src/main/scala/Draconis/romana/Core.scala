package Draconis.romana

import org.apache.commons.io.FileUtils
import java.io.File

object Core {

  private[this] def readF(path: String): IndexedSeq[String] = FileUtils.
    readFileToString(new File(path),"UTF-8").split("\n").toIndexedSeq

  private[this] def writeF(path: String, data: String, app: Boolean): Unit = FileUtils.
    write(new File(path), data, "UTF-8", app)

  def getModules: IndexedSeq[String] =
    readF(System.getProperty("user.home") + "/Draconis/modules.txt")

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

  def evaluate(newSp: List[(String, String)], newCh: List[(String, List[(String, Boolean)])],
    leftRi: List[(String, String)], id: String): Unit = {
    val path = System.getProperty("user.home") + "/Draconis/" + Holder.getModule + "/"
    val u =  Holder.getUser.username
    var xx = 0
    readF(path + "dictionary.txt").foreach( x => if (x contains id) xx += 1 )

    var brute = ""

    var total = 0
    var score = 0


    val ultCh = newCh.map( x => {
      x._1 -> x._2.map( y => {
        if ( y._2 ) y._1 + "#"
        else y._1
      })
    })
    val nor = Holder.getExactTest(id)

    nor match {
      case Some(x) =>
        total = x.puncte
        for ( alfa <- x.problems ){
          if ( alfa.tip == "C##E" ){
            for {
              beta <- alfa.exercitii
              ss <- newSp
              if ss._1 == beta._1
              if ss._2 != ""
            } if ( beta._2.split("[,]").map(_.toLowerCase.trim ) contains ss._2.toLowerCase.trim ){
              brute = brute + "C##E:V:" + beta._1 + "##" + ss._2.toLowerCase.trim + "\n"
              score += alfa.score / alfa.exercitii.size
            }
            else brute = brute + "C##E:P:" + beta._1 + "##" + ss._2.toLowerCase.trim + "\n"
          }
          else if ( alfa.tip == "D##D" ){
            for {
              beta <- alfa.exercitii
              ss <- leftRi
              if beta._1 == ss._1
            } if ( beta._2 == ss._2 ){
              brute = brute + "D##D:V:" + beta._1 + "##" + ss._2 + "\n"
              score += alfa.score / alfa.exercitii.size
            }
            else brute = brute + "D##D:P:" + beta._1 + "##" + ss._2 + "\n"
          }
          else if ( alfa.tip == "C##V" ){
            for {
              beta <- alfa.exercitii
              ss <- ultCh
              if beta._1 == ss._1
            } for {
              gamma <- ss._2
            } if ( beta._2.split("[,]") contains gamma )
            brute = brute + "C##V:V:" + beta._1 + "##" + gamma + "\n"
            else brute = brute + "C##V:P:" + beta._1 + "##" + gamma + "\n"
          }
        }
      case None => {}
    }

    var toCheck = List[String]()
    brute.split("\n").foreach( x => {
      if ( x.startsWith("C##V:V:") && !toCheck.contains(x.drop(7).split("##")(0)) ){
        toCheck = x.drop(7).split("##")(0) :: toCheck
      }
    })

    toCheck.foreach( x => {
      var good = true
      brute.split("\n").foreach( y => {
        if ((y contains x) && (y contains "C##V:P:")) good = false
      })
      if ( good ){
        nor match {
          case Some(z) =>
            for {
              alfa <- z.problems
              beta <- alfa.exercitii
              if beta._1 == x
            } score += alfa.score / alfa.exercitii.size
          case None => {}
        }
      }
    })

    val secTot = s"${score}/${total}"
    writeF(path + "dictionary.txt", s"T-${id}-${xx}-${u}-${secTot}\n", true)
    writeF(path + "/progress/" + s"T-${id}-${xx}-${u}-${score}-${total}", s"${brute}\n\n${secTot}", false)
  }

}
