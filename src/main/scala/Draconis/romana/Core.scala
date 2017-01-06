package Draconis.romana

import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.mutable.ListBuffer

object Core {

  private[this] val home = System.getProperty("user.home") + "/Draconis/"

  private[this] def readF(path: String): IndexedSeq[String] = FileUtils.
    readFileToString(new File(path),"UTF-8").split("\n").toIndexedSeq

  private[this] def writeF(path: String, data: String, app: Boolean): Unit = FileUtils.
    write(new File(path), data, "UTF-8", app)

  def getModules: IndexedSeq[String] =
    readF(home + "modules.txt")

  def main(args: Array[String]): Unit = {
    Holder.loadUsers(readF(home + "users.txt"))
    Frame.initial
  }

  def initModule(s: String): Unit = {
    Holder.setModule(s)

    readF(home + s + "/settings.txt").foreach(x => {
      val Array(s1, s2, s3) = x.split("[=]")
      if (s1 == "1") Holder.setSettings(s2 -> s3)
      else {
        val path = home + s
        val (one, two) = (s3.split("[,]")(0), s3.split("[,]")(1).toInt)
        if (s1 == "2")
          Holder.addLectura(readF(path + "/material/" + s2), one, two)
        else if (s1 == "3")
          Holder.addTest(readF(path + "/test/" + s2), one, two)
        else if (s1 == "4")
          Holder.addGallery(one, two)
      }
    })

    calculateStats
  }

  def register(us: String, pa: String, nu: String, pr: String, sc: String,
    opt: String, isElev: Boolean): Unit = {
    var sp = if (isElev) "e" else "p"
    val link = s"$us,$pa,$nu,$pr,$sc,$opt,$sp"

    Holder.loadUsers(IndexedSeq(link))
    writeF(home + "users.txt", link + "\n", true)
  }

  def evaluate(newSp: List[(String, String)], newCh: List[(String, List[(String, Boolean)])],
    leftRi: List[(String, String)], id: String): Unit = {
    val path = home + Holder.getModule + "/"
    val u =  Holder.getUser.username
    var xx = 0
    var total = 0.0
    var score = 0.0
    var brute = ""

    readF(path + "dictionary.txt").foreach( x => if (x contains id) xx += 1 )

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
            } if ( beta._2.split("[,]").map(_.toLowerCase.trim ) contains ss._2.toLowerCase.trim )
              brute = brute + "C##E:V:" + beta._1 + "##" + ss._2.toLowerCase.trim + "\n"
            else brute = brute + "C##E:P:" + beta._1 + "##" + ss._2.toLowerCase.trim + "\n"
          }
          else if ( alfa.tip == "D##D" ){
            for {
              beta <- alfa.exercitii
              ss <- leftRi
              if beta._1 == ss._1
            } if ( beta._2 == ss._2 )
              brute = brute + "D##D:V:" + beta._1 + "##" + ss._2 + "\n"
            else brute = brute + "D##D:P:" + beta._1 + "##" + ss._2 + "\n"
          }
          else if ( alfa.tip == "C##V" ){
            for {
              beta <- alfa.exercitii
              ss <- ultCh
              if beta._1 == ss._1
            }{
              var exact = ""
              var good = true
              for ( gamma <- ss._2 ){
                if ( !(beta._2.split("[,]") contains gamma) )
                  good = false
                exact = s"${gamma},${exact}"
              }
              if ( good ) brute = brute + "C##V:V:" + beta._1 + "##" + exact + "\n"
              else brute = brute + "C##V:P:" + beta._1 + "##" + exact + "\n"
            }
          }
        }
      case None => {}
    }
    
    val processed = {
      var sweet = brute
      nor match {
        case Some(x) =>
          for {
            alfa <- x.problems
            beta <- alfa.exercitii
            if !(brute.split("\n").map(x=>x.drop(7).split("##")(0)) contains beta._1)
          } sweet = s"${sweet}${alfa.tip}:P:${beta._1}\n"
        case None => {}
      }
      sweet
    }

    nor match {
      case Some(x) =>
        for {
          alfa <- brute.split("\n")
          beta <- x.problems
          gamma <- beta.exercitii
          if gamma._1 == alfa.drop(7).split("##")(0)
          if alfa.drop(5).startsWith("V:")
        } score += beta.score / beta.exercitii.size
      case None => {}
    }

    val secTot = s"${score}/${total}"
    writeF(s"${path}dictionary.txt", s"T#${id}#${xx}#${u}#${secTot}\n", true)
    writeF(s"${path}/progress/T--${id}--${xx}--${u}", processed, false)

    calculateStats
  }

  private[this] def calculateStats(): Unit = {
    val finals = ListBuffer[String]()
    val entries: ListBuffer[String] = readF(s"${home}${Holder.getModule}/dictionary.txt")
    .to[ListBuffer].filter(_ != "").filter(_.split("#")(3) == Holder.getUser.username)

    for ( alfa <- 0 until entries.size ){
      var suite = entries(alfa)
      for ( beta <- alfa + 1 until entries.size ){
        if ( entries(beta).split("#")(1) == suite.split("#")(1) &&
          (entries(beta).split("#")(4).split("[/]")(0).toDouble >
          suite.split("#")(4).split("[/]")(0).toDouble) )

          suite = entries(beta)
      }

      var toGive = true
      for ( c <- finals ) if ( c.split("#")(1) == suite.split("#")(1) ) toGive = false
      if ( toGive ) finals += suite
    }

    val percent: Double = finals.size.toDouble / Holder.getTests.size.toDouble * 100.0
    val scores = finals.map( y => {
      val c = y.split("#")(4).split("[/]")
      (c(0).toDouble / c(1).toDouble * 100.0)
    })
    var sum: Double = 0.0
    var n: Double = 0.0
    scores.foreach( x => {
      sum += x
      n += 1
    })

    val tempo = if ( (sum/n).toString == "NaN" ) "0" else (sum/n).toString
    Holder.setStats(percent.toString, tempo)
    Frame.refreshElev
  }

  def testInstances(n: String): List[(String, String)] = readF(s"${home}${Holder.getModule}/dictionary.txt")
    .to[List].filter(_ != "")
    .filter(_.startsWith("T"))
    .filter(_.split("#")(3) == Holder.getUser.username)
    .filter(_.split("#")(1) == n)
    .sortWith(_.split("#")(2).split("[/]")(0).toDouble > _.split("#")(2).split("[/]")(0).toDouble)
    .map( x => s"${x.split("#")(1)} | ${x.split("#")(4)}" -> x.split("#")(2) )

  def getResults(n: String, d: String): IndexedSeq[String] = {
    readF(s"${home}${Holder.getModule}/progress/T--${n}--${d}--${Holder.getUser.username}")
  }

}
