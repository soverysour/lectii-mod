package Draconis.romana

import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.mutable.ListBuffer

import Defaults.Paths._
import Defaults.Names._
import Defaults.Misc._
import Defaults.ProcessDictionaryEntries._
import Defaults.ProcessTestEntries._
import Defaults.ProcessAccountsSettings._

object Core {

  private[this] def readF(path: String): Array[String] = FileUtils
    .readFileToString(new File(path), encoding).split("\n")
  private[this] def writeF(path: String, data: String, append: Boolean): Unit = FileUtils
    .write(new File(path), data, encoding, append)

  def main(args: Array[String]): Unit = {
    if ( readF(typePath).mkString == studentMark ){
      Holder.loadUsers(readF(usersPath).filter(a_isProperEntry(_)))
      val alfa = readF(modulePath).map( x => {
        s_splitData(x)(0).trim -> s_splitData(x)(1).trim
      }).toMap
      Holder.loadModules(alfa)
      Holder.setType(studentMark)
    }
    else Holder.setType(professorMark)

    Frame.initial()
  }

  def initModule(m: String): Unit = {
    Holder.setModule(m)

    readF( settingsPath ).filter(s_isProperEntry(_)).foreach(x => {
      val Array(form, name, parameters) = s_splitData(x)
      val (fullName, level) = s_getName(parameters) -> s_getLevel(parameters)
      if (form == materialName)
        Holder.addMaterial(readF(s"${materialPath}${name}"), fullName, level)
      else if (form == testName)
        Holder.addTest(readF(s"${testPath}${name}"), fullName, level)
      }
    )
    calculateStats
  }

  def register(us: String, pa: String, nu: String, pr: String, sc: String, opt: String): Unit = {
    val link = a_formatData(List(us, pa, nu, pr, sc, opt))

    Holder.loadUsers(Array(link))
    writeF(usersPath, s"\n${link}\n", true)
  }

  def evaluate(newSp: List[(String, String)], newCh: List[(String, List[(String, Boolean)])],
    leftRi: List[(String, String)], id: String): Unit = {
    val username =  Holder.getUser.username
    var discr = 0
    var total = 0.0
    var score = 0.0
    var info = ""

    readF(dictionaryPath).filter( _ != "" ).foreach( x => if (d_getId(x) == id) discr += 1 )

    val finalCheckboxes = newCh.map( x => {
      x._1 -> x._2.map( y => {
        if ( y._2 ) y._1 + defaultIdentifier
        else y._1
      })
    })

    val exactTest = Holder.getExactTest(id)
    total = exactTest.points
    for ( problem <- exactTest.problems ){
      if ( problem.kind == completeSpace ){
        for {
          workSample <- problem.workload
          solution <- newSp
          if solution._1 == workSample._1
          if solution._2 != ""
        } if ( t_splitAns(workSample._2).map(m_prettify(_)) contains m_prettify(solution._2) ){
          info = t_format(info, workSample._1, m_prettify(solution._2), true, completeSpace)
          score += problem.score / problem.size
        }
        else info = t_format(info, workSample._1, m_prettify(solution._2), false, completeSpace)
      }
      else if ( problem.kind == dragDrop ){
        for {
          workSample <- problem.workload
          solution <- leftRi
          if workSample._1 == solution._1
        } if ( workSample._2 == solution._2 ){
          info = t_format(info, workSample._1, solution._2, true, dragDrop)
          score += problem.score / problem.size
        }
        else info = t_format(info, workSample._1, solution._2, false, dragDrop)
      }
      else if ( problem.kind == chooseVariant ){
        for {
          workSample <- problem.workload
          solution <- finalCheckboxes
          if workSample._1 == solution._1
        } {
          var exact = ""
          var good = true
          for ( option <- solution._2 ){
            if ( !(t_splitAns(workSample._2) contains option) )
              good = false
            exact = s"${option}${defaultSeparator}${exact}"
          }
          exact = exact.dropRight(defaultSeparator.size)
          if ( good ){
            info = t_format(info, workSample._1, exact, true, chooseVariant)
            score += problem.score / problem.size
          }
          else info = t_format(info, workSample._1, exact, false, chooseVariant)
        }
      }
    }

    for {
      problem <- exactTest.problems
      workSample <- problem.workload
      if ( (!(info.split("\n").map(x=> t_getName(x)) contains workSample._1)) && (workSample._1 != nonExisting && workSample._2 != nonExisting))
    } info = t_format(info, workSample._1, "", false, problem.kind)

    val scoreRatio = d_formatScore(score, total)
    writeF(dictionaryPath, d_formatEntry(id, discr, scoreRatio), true)
    writeF(d_formatPath(id, discr), info, false)

    calculateStats
  }

  private[this] def calculateStats(): Unit = {
    val finals = ListBuffer[String]()
    val entries = readF(dictionaryPath)
      .to[ListBuffer]
      .filter(d_isProperEntry(_))
      .filter(d_getUser(_) == Holder.getUser.username)

    for ( alfa <- 0 until entries.size ){
      var suite = entries(alfa)
      for ( beta <- alfa + 1 until entries.size ){
        if ( d_getId(entries(beta)) == d_getId(suite) && d_getScore(entries(beta)) > d_getScore(suite) )
          suite = entries(beta)
      }

      var toGive = true
      for ( c <- finals ) if ( d_getId(c) == d_getId(suite) ) toGive = false
      if ( toGive ) finals += suite
    }

    val percent = (finals.size.toDouble / Holder.getTests.size.toDouble * 100.0).toString
    val scores = finals.map( y => d_getScore(y) / d_getTotal(y) * 100.0 )

    var sum = 0.0
    var n = 0.0
    scores.foreach( x => {
      sum += x
      n += 1
    })

    Holder.setStats(percent, m_verifyAmount(sum, n))
    Frame.refreshElev()
  }

  def testInstances(n: String): List[(String, String)] = readF(dictionaryPath)
    .filter(_ != "")
    .filter(d_getType(_) == testName)
    .filter(d_getUser(_) == Holder.getUser.username)
    .filter(d_getId(_) == n)
    .sortWith(d_getDiscr(_) > d_getDiscr(_))
    .map( x => s"${d_getId(x)} | ${d_getResult(x)}" -> d_getDiscr(x) )
    .to[List]

  def getResults(testName: String, discr: String): Array[String] = {
    readF(d_formatPath(testName, discr.toInt))
  }

}
