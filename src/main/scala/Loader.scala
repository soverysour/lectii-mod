import scala.collection.mutable.{Map, Set}
import scala.io.Source
import java.io.FileWriter

object Loader {
    private[this] var users: Map[String, Cont] = Map[String, Cont]()
    private[this] var currentUser: Cont = null

    private[this] var settings: Map[String, String] = Map[String, String]()
    private[this] var info: Set[Lectura] = Set[Lectura]()
    private[this] var tests: Set[Test] = Set[Test]()

    //Load all the profiles into the users map. This is always done regardless of chosen Module.
    def init: Unit = {
        val profiles = Source.fromFile(System.getProperty("user.home")+"\\Draconis\\users.txt")
        profiles.getLines.toIndexedSeq.foreach( x => {
            val e = x.split("[,]")
            users += (e(0) -> new Cont(e(0), e(1), e(2), e(3), e(4), e(5), e(6)))
        })
        profiles.close
    }

    //Get value of a setting inside a module or the password for a user (independent of module).
    def getSettings(arg: String): String = settings(arg)
    def getLog(arg: String): String = {
        try { users(arg).parola }
        catch { case _: Throwable => "" }
    }
    def setUser(arg: String): Unit = { currentUser = users(arg) }
    def getUser: Cont = currentUser

    //Register a new account by adding it to the users map and writing it to the users.txt.
    def register(us: String, pa: String, nu: String, pr: String, sc: String, opt: String, isElev: Boolean): Unit = {
        var sp = if ( isElev ) "e" else "p"

        users += (us -> new Cont(us, pa, nu, pr, sc, opt, sp) )

        val fw = new FileWriter(System.getProperty("user.home") + "\\Draconis\\users.txt", true)
        try { fw.write("\n" + us +","+ pa +","+ nu +","+ pr +","+ sc +","+ opt +","+ sp) }
        finally fw.close
    }

    //Data type for holding tests, their content and their settings.
    class Test(sourceTest: IndexedSeq[String], val name: String, val level: Int){
        class Exercitiu(val tip: String, val ex: Set[String]){}
        private[this] var subiect = Set[Exercitiu]()
        private[this] var exer = new Exercitiu("#", Set())
        for ( x <- 0 until sourceTest.size ){
            if ( sourceTest(x) == "##" ) subiect += new Exercitiu(exer.tip, exer.ex)
            else if ( sourceTest(x).replaceFirst("[CD][#]{2}[EDV]", "#") == "#" ) 
                exer = new Exercitiu(sourceTest(x), Set())
            else exer = new Exercitiu(exer.tip, exer.ex + sourceTest(x) )
        }

        val problems = subiect
    }

    //Data type for holding materials, their content and settings.
    class Lectura(sourceTest: IndexedSeq[String], val name: String, val level: Int){
        val info = (for ( x <- 0 until sourceTest.size ) yield sourceTest(x) + "\n" ).mkString
    }

    //Data type that holds both students and admin level account details.
    class Cont(val username: String, val parola: String, val nume: String, val prenume: String, val scoala: String, val opttext: String, val elev: String){ val isElev = elev == "e" }
    
}