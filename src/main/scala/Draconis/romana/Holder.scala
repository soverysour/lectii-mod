package Draconis.romana

import scala.collection.mutable.{ Map, Set }
import Defaults.ProcessAccountsSettings.a_splitData
import Defaults.ProcessRawTest.r_isHeader
import Defaults.Names._

object Holder {
  private[this] var users: Map[String, Cont] = Map()
  private[this] var settings: Map[String, String] = Map()
  private[this] var currentUser: Cont = _
  private[this] var currentModule: String = _
  private[this] var moduleList: Array[String] = _
  private[this] var stats: (String, String) = _

  private[this] var info: Set[Material] = _
  private[this] var tests: Set[Test] = _

  def loadUsers(profiles: Array[String]): Unit = {
    profiles.foreach(x => {
      val e = a_splitData(x)
      users += (e(id_username) -> new Cont(e(id_username), e(id_password), e(id_surname), e(id_name), e(id_school), e(id_optional), e(id_status) == studentMark))
    })
  }
  def loadModules(s: Array[String]): Unit = moduleList = s

  def getModules: Array[String] = moduleList
  def getModule: String = currentModule
  def setModule(m: String): Unit = {
    currentModule = m

    settings = Map()
    info = Set()
    tests = Set()
  }
  def getUser: Cont = currentUser
  def setUser(arg: String): Unit = { currentUser = users(arg) }
  def getSettings(arg: String): String = settings(arg)
  def setSettings(s: (String, String)): Unit = { settings += s }
  def getLog(arg: String): String = {
    try { users(arg).password }
    catch { case _: Throwable => "" }
  }

  def getStats: (String, String) = stats
  def setStats(left: String, right: String): Unit = stats = left -> right

  def getInfo: List[Material] = info.toList.sortWith(sortElem)
  def getTests: List[Test] = tests.toList.sortWith(sortElem)

  def getExactInfo(name: String): Material = {
    info.foreach(x => if (x.name == name) return x)
    new Material(Array(), "", 0)
  }
  def getExactTest(name: String): Test = {
    tests.foreach(x => if (x.name == name) return x)
    new Test(Array(), "", 0)
  }

  def addMaterial(i: Array[String], name: String, level: Int): Unit = { info += new Material(i, name, level) }
  def addTest(i: Array[String], name: String, level: Int): Unit = { tests += new Test(i, name, level) }

  private[this] def sortElem(x1: ToSort, x2: ToSort): Boolean = {
    if (x1.level == x2.level) x1.name < x2.name
    else x1.level < x2.level
  }

  sealed trait ToSort {
    val level: Int
    val name: String
  }

  class Test(sourceTest: Array[String], nume: String, nivel: Int) extends ToSort {
    sealed class Exercitiu(gen: String, val ex: Set[String], sc: String) {
      val kind = gen
      val workload = for (x <- ex) yield (x.split(powerSeparator)(0), x.split(powerSeparator)(1))
      val score = sc.toInt
    }

    private[this] var subiect = Set[Exercitiu]()
    private[this] var exer = new Exercitiu("", Set(), "0")
    private[this] var pp = 0

    for (x <- 0 until sourceTest.size) {
      if (sourceTest(x).startsWith(defaultSeparator))
        subiect += new Exercitiu(exer.kind, exer.ex, sourceTest(x).drop(2))
      else if (r_isHeader(sourceTest(x)))
        exer = new Exercitiu(sourceTest(x), Set(), "0")
      else exer = new Exercitiu(exer.kind, exer.ex + sourceTest(x), "0")
    }
    subiect.foreach(pp += _.score)

    val problems = subiect
    val points = pp
    override val level: Int = nivel
    override val name: String = nume
  }

  class Material(sourceTest: Array[String], nume: String, nivel: Int) extends ToSort {
    val info = (for (x <- sourceTest) yield x + "\n").mkString
    override val level: Int = nivel
    override val name: String = nume
  }

  class Cont(val username: String, val password: String, val surname: String, val name: String,
    val school: String, val optText: String, val isStudent: Boolean)
}
