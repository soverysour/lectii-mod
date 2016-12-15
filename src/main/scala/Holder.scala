import scala.collection.mutable.{Map, Set}
import scala.io.Source
import java.io.{File, FileWriter}

object Holder {
    private[this] var users: Map[String, Cont] = Map[String, Cont]()
    private[this] var settings: Map[String, String] = Map[String, String]()
    private[this] var currentUser: Cont = null
    private[this] var currentModule: String = ""

    private[this] var info: Set[Lectura] = Set[Lectura]()
    private[this] var tests: Set[Test] = Set[Test]()
    private[this] var gallery: Set[Gallery] = Set[Gallery]()

    //Load all the profiles into the users map. This is always done regardless of chosen Module.
    def init: Unit = {
        val profiles = Source.fromFile(System.getProperty("user.home")+"\\Draconis\\users.txt")
        profiles.getLines.toIndexedSeq.foreach( x => {
            val e = x.split("[,]")
            users += (e(0) -> new Cont(e(0), e(1), e(2), e(3), e(4), e(5), e(6) == "e"))
        })
        profiles.close
    }

    //The loading that is done after the user selects a module.
    def load(s: String): Unit = {
        currentModule = s
        val genSet = Source.fromFile(System.getProperty("user.home")+"\\Draconis\\"+s+"\\settings.txt")
        genSet.getLines.toIndexedSeq.foreach( x => {
            val ss = x.split("[=]")
            readNload(ss(0), ss(1), ss(2))
        })
        genSet.close
    }

    //Get value of a setting inside a module or the password for a user (independent of module) 
    //or set the currentUser or get the currentUser.
    def getUser: Cont = currentUser
    def setUser(arg: String): Unit = { currentUser = users(arg) }
    def getSettings(arg: String): String = settings(arg)
    def getLog(arg: String): String = {
        try { users(arg).parola }
        catch { case _: Throwable => "" }
    }
    def getModules: List[String] = new File(System.getProperty("user.home")+"\\Draconis\\").
        listFiles.filter(_.isDirectory).toList.map(_.getName)

    //Getters for info, sets and gallery entries.
    def getInfo: List[Lectura] = info.toList.sortWith(sortElem)
    def getTests: List[Test] = tests.toList.sortWith(sortElem)
    def getGallery: List[Gallery] = gallery.toList.sortWith(sortElem)

    //Exact getters for info, sets and gallery entries.
    def getExactInfo(name: String): Option[Lectura] = {
        info.foreach( x => if ( x.nume == name ) return Some(x) )
        None
    }
    def getExactTest(name: String): Option[Test] = {
        tests.foreach( x => if ( x.nume == name ) return Some(x) )
        None
    }
    def getExactGallery(name: String): Option[Gallery] = {
        gallery.foreach( x => if ( x.nume == name ) return Some(x) )
        None
    }

    //Register a new account by adding it to the users map and writing it to the users.txt.
    def register(us: String, pa: String, nu: String, pr: String, sc: String,
                 opt: String, isElev: Boolean): Unit = {
        var sp = if ( isElev ) "e" else "p"

        users += (us -> new Cont(us, pa, nu, pr, sc, opt, isElev) )

        val fw = new FileWriter(System.getProperty("user.home") + "\\Draconis\\users.txt", true)
        try { fw.write("\n" + us +","+ pa +","+ nu +","+ pr +","+ sc +","+ opt +","+ sp) }
        finally fw.close
    }

    //Sorting method for materials, tests and gallery entries.
    private[this] def sortElem(x1: ToSort, x2: ToSort): Boolean = {
        if ( x1.nivel == x2.nivel ) x1.nume < x2.nume
        else x1.nivel < x2.nivel
    }

    //To facilitate sorting.
    sealed trait ToSort {
        val nivel: Int
        val nume: String
    }

    //Data type for holding tests, their content and their settings.
    class Test(sourceTest: IndexedSeq[String], name: String, level: Int) extends ToSort {
        sealed class Exercitiu(val tip: String, val ex: Set[String]){
            override def toString: String = {
                ex foreach println
                tip
            }
        }
        private[this] var subiect = Set[Exercitiu]()
        private[this] var exer = new Exercitiu("#", Set())
        for ( x <- 0 until sourceTest.size ){
            if ( sourceTest(x) == "##" ) subiect += new Exercitiu(exer.tip, exer.ex)
            else if ( sourceTest(x).replaceFirst("[CD][#]{2}[EDV]", "#") == "#" ) 
                exer = new Exercitiu(sourceTest(x), Set())
            else exer = new Exercitiu(exer.tip, exer.ex + sourceTest(x) )
        }

        val problems = subiect
        override val nivel: Int = level
        override val nume: String = name

        override def toString: String = {
            problems foreach println
            name + level
        }
    }

    //Data type for holding materials, their content and settings.
    class Lectura(sourceTest: IndexedSeq[String], name: String, level: Int) extends ToSort {
        val info = (for ( x <- 0 until sourceTest.size ) yield sourceTest(x) + "\n" ).mkString
        override val nivel: Int = level
        override val nume: String = name

        override def toString: String = name + level + info
    }

    //Data structure for holding gallery entrances.
    class Gallery(val name: String, val level: Int) extends ToSort {
        override val nivel: Int = level
        override val nume: String = name
    }

    //Data type that holds both students and admin level account details.
    class Cont(val username: String, val parola: String, val nume: String, val prenume: String,
               val scoala: String, val opttext: String, val isElev: Boolean){
        override def toString: String = { username + " " + parola + " " + nume + " " + prenume + " " +
                                          scoala + " " + opttext + " " + isElev }
    }
    
    //From the settings file, either adds a setting, a test or a material.
    private[this] def readNload(s1: String, s2: String, s3: String): Unit = {
        if ( s1 == "1" ) settings += ( s2 -> s3 )
        else {
            val path = System.getProperty("user.home")+"\\Draconis\\"+currentModule+"\\"
            val (one, two) = (s3.split("[,]")(0), s3.split("[,]")(1).toInt)
            if ( s1 == "2" ){
                val here = Source.fromFile(path+"material\\"+s2)
                info += new Lectura(here.getLines.toIndexedSeq, one, two)
                here.close
            }
            else if ( s1 == "3" ){
                val here = Source.fromFile(path+"tests\\"+s2)
                tests += new Test(here.getLines.toIndexedSeq, one, two)
                here.close
            }
            else {
                val here = Source.fromFile(path+"tests\\"+s2)
                gallery += new Gallery(one, two)
                here.close
            }
        }
    }

    private[this] def runCheckings: Unit = {
        println;println;println;println("COMMENCING")
        for ( (k,v) <- users ) println(k + " --> " + v )
        for ( (k,v) <- settings ) println(k + " --> " + v )

        println(currentUser)
        println(currentModule)

        info foreach println
        tests foreach println

        println;println;println;println("OVER")
    }
}