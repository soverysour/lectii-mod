package Draconis.romana

import scala.collection.mutable.{ Map, Set }

object Holder {
  private[this] var users: Map[String, Cont] = Map[String, Cont]()
  private[this] var settings: Map[String, String] = Map[String, String]()
  private[this] var currentUser: Cont = _
  private[this] var currentModule: String = _

  private[this] var info: Set[Lectura] = Set[Lectura]()
  private[this] var tests: Set[Test] = Set[Test]()
  private[this] var gallery: Set[Gallery] = Set[Gallery]()

  def loadUsers(profiles: IndexedSeq[String]): Unit = {
    profiles.foreach(x => {
      val e = x.split("[,]")
      users += (e(0) -> new Cont(e(0), e(1), e(2), e(3), e(4), e(5), e(6) == "e"))
    })
  }

  def setModule(m: String): Unit = { currentModule = m }
  def getModule: String = currentModule
  def getUser: Cont = currentUser
  def setUser(arg: String): Unit = { currentUser = users(arg) }
  def getSettings(arg: String): String = settings(arg)
  def setSettings(s: (String, String)): Unit = { settings += s }
  def getLog(arg: String): String = {
    try { users(arg).parola }
    catch { case _: Throwable => "" }
  }

  def getInfo: List[Lectura] = info.toList.sortWith(sortElem)
  def getTests: List[Test] = tests.toList.sortWith(sortElem)
  def getGallery: List[Gallery] = gallery.toList.sortWith(sortElem)

  def getExactInfo(name: String): Option[Lectura] = {
    info.foreach(x => if (x.nume == name) return Some(x))
    None
  }
  def getExactTest(name: String): Option[Test] = {
    tests.foreach(x => if (x.nume == name) return Some(x))
    None
  }
  def getExactGallery(name: String): Option[Gallery] = {
    gallery.foreach(x => if (x.nume == name) return Some(x))
    None
  }

  def addLectura(i: IndexedSeq[String], nu: String, ni: Int): Unit = { info += new Lectura(i, nu, ni) }
  def addTest(i: IndexedSeq[String], nu: String, ni: Int): Unit = { tests += new Test(i, nu, ni) }
  def addGallery(nu: String, ni: Int): Unit = { gallery += new Gallery(nu, ni) }

  private[this] def sortElem(x1: ToSort, x2: ToSort): Boolean = {
    if (x1.nivel == x2.nivel) x1.nume < x2.nume
    else x1.nivel < x2.nivel
  }

  sealed trait ToSort {
    val nivel: Int
    val nume: String
  }

  class Test(sourceTest: IndexedSeq[String], name: String, level: Int) extends ToSort {

    sealed class Exercitiu(gen: String, val ex: Set[String], sc: String) {
      val tip = gen
      val exercitii = for (x <- ex) yield (x.split("##")(0), x.split("##")(1))
      val score = sc.toInt
    }

    private[this] var subiect = Set[Exercitiu]()
    private[this] var exer = new Exercitiu("#", Set(), "0")
    private[this] var pp = 0

    for (x <- 0 until sourceTest.size) {
      if (sourceTest(x).startsWith("##")) subiect += new Exercitiu(exer.tip, exer.ex, sourceTest(x).drop(2))
      else if (sourceTest(x).replaceFirst("[CD]##[EDV]", "#") == "#")
        exer = new Exercitiu(sourceTest(x), Set(), "0")
      else exer = new Exercitiu(exer.tip, exer.ex + sourceTest(x), "0")
    }
    subiect.foreach(pp += _.score)

    val problems = subiect
    val puncte = pp
    override val nivel: Int = level
    override val nume: String = name
  }

  class Lectura(sourceTest: IndexedSeq[String], name: String, level: Int) extends ToSort {
    val info = (for (x <- 0 until sourceTest.size) yield sourceTest(x) + "\n").mkString
    override val nivel: Int = level
    override val nume: String = name
  }

  class Gallery(val name: String, val level: Int) extends ToSort {
    override val nivel: Int = level
    override val nume: String = name
  }

  class Cont(val username: String, val parola: String, val nume: String, val prenume: String,
    val scoala: String, val opttext: String, val isElev: Boolean)
}
