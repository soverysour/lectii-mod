package Draconis.romana

import scala.collection.mutable.{ Map, Set }
import Defaults.ProcessAccountsSettings.a_splitData
import Defaults.ProcessRawTest.r_isHeader
import Defaults.Names.{ elevMark, problemEnd, testSeparator }

object Holder {
  private[this] var users: Map[String, Cont] = Map()
  private[this] var settings: Map[String, String] = Map()
  private[this] var currentUser: Cont = _
  private[this] var currentModule: String = _
  private[this] var moduleList: Array[String] = _
  private[this] var stats: (String, String) = _

  private[this] var info: Set[Material] = Set()
  private[this] var tests: Set[Test] = Set()
  private[this] var gallery: Set[Gallery] = Set()

  def loadUsers(profiles: Array[String]): Unit = {
    profiles.foreach(x => {
      val e = a_splitData(x)
      users += (e(0) -> new Cont(e(0), e(1), e(2), e(3), e(4), e(5), e(6) == elevMark))
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
    gallery = Set()
  }
  def getUser: Cont = currentUser
  def setUser(arg: String): Unit = { currentUser = users(arg) }
  def getSettings(arg: String): String = settings(arg)
  def setSettings(s: (String, String)): Unit = { settings += s }
  def getLog(arg: String): String = {
    try { users(arg).parola }
    catch { case _: Throwable => "" }
  }

  def getStats: (String, String) = stats
  def setStats(left: String, right: String): Unit = stats = left -> right

  def getInfo: List[Material] = info.toList.sortWith(sortElem)
  def getTests: List[Test] = tests.toList.sortWith(sortElem)
  def getGallery: List[Gallery] = gallery.toList.sortWith(sortElem)

  def getExactInfo(name: String): Material = {
    info.foreach(x => if (x.nume == name) return x)
    new Material(Array(), "", 0)
  }
  def getExactTest(name: String): Test = {
    tests.foreach(x => if (x.nume == name) return x)
    new Test(Array(), "", 0)
  }
  def getExactGallery(name: String): Gallery = {
    gallery.foreach(x => if (x.nume == name) return x)
    new Gallery("", 0)
  }

  def addMaterial(i: Array[String], nu: String, ni: Int): Unit = { info += new Material(i, nu, ni) }
  def addTest(i: Array[String], nu: String, ni: Int): Unit = { tests += new Test(i, nu, ni) }
  def addGallery(nu: String, ni: Int): Unit = { gallery += new Gallery(nu, ni) }

  private[this] def sortElem(x1: ToSort, x2: ToSort): Boolean = {
    if (x1.nivel == x2.nivel) x1.nume < x2.nume
    else x1.nivel < x2.nivel
  }

  sealed trait ToSort {
    val nivel: Int
    val nume: String
  }

  class Test(sourceTest: Array[String], name: String, level: Int) extends ToSort {
    sealed class Exercitiu(gen: String, val ex: Set[String], sc: String) {
      val kind = gen
      val workload = for (x <- ex) yield (x.split(testSeparator)(0), x.split(testSeparator)(1))
      val score = sc.toInt
    }

    private[this] var subiect = Set[Exercitiu]()
    private[this] var exer = new Exercitiu("", Set(), "0")
    private[this] var pp = 0

    for (x <- 0 until sourceTest.size) {
      if (sourceTest(x).startsWith(problemEnd))
        subiect += new Exercitiu(exer.kind, exer.ex, sourceTest(x).drop(2))
      else if (r_isHeader(sourceTest(x)))
        exer = new Exercitiu(sourceTest(x), Set(), "0")
      else exer = new Exercitiu(exer.kind, exer.ex + sourceTest(x), "0")
    }
    subiect.foreach(pp += _.score)

    val problems = subiect
    val points = pp
    override val nivel: Int = level
    override val nume: String = name
  }

  class Material(sourceTest: Array[String], name: String, level: Int) extends ToSort {
    val info = (for (x <- sourceTest) yield x + "\n").mkString
    override val nivel: Int = level
    override val nume: String = name
  }

  class Gallery(name: String, level: Int) extends ToSort {
    override val nivel: Int = level
    override val nume: String = name
  }

  class Cont(val username: String, val parola: String, val nume: String, val prenume: String,
    val scoala: String, val opttext: String, val isElev: Boolean)
}
