import scala.swing._
import scala.collection.mutable.{Map, Set}
import scala.io.Source
import java.io.{File, FileWriter}

object Loader {
    private[this] var elevi: Map[String, Elev] = Map[String, Elev]()
    private[this] var profesori: Map[String, Profesor] = Map[String, Profesor]()
    private[this] var utilizatori: Map[String, String] = Map[String, String]()
    private[this] var settings: Map[String, String] = Map[String, String]()
    private[this] var info: Set[Lectura] = Set[Lectura]()
    private[this] var tests: Set[Test] = Set[Test]()

    def init: Unit = {
        val path = System.getProperty("user.home") + "\\Draconis\\"

        val profiles = Source.fromFile(path+"users.txt")
        profiles.getLines.toIndexedSeq.foreach( x => {
            if ( x.contains("[") ){
                val name = x.split("\\[")(0)
                val right = x.split("\\[")(1).split("[,]")
                profesori += (name -> new Profesor(right(0), right(1), right(2), right(3), right(4)))
            }
            else {
                val name = x.split("\\{")(0)
                val right = x.split("\\{")(1).split("[,]")
                elevi += (name -> new Elev(right(0),right(1),right(2),right(3),right(4),right(5).toInt))
            }
        })
        profiles.close

        val sets = Source.fromFile(path+"setari.txt")
        sets.getLines.toIndexedSeq.foreach( x => {
            if ( x.contains("{") ){
                val file = x.split("[{]")(0)
                val right = x.split("[{]")(1).split("[,]")
                val isMat = right(0) == "mat"
                val name = right(1)
                val level = right(2).toInt

                if ( isMat ) {
                    val content = Source.fromFile(path+"material\\"+file)
                    info += new Lectura(content.getLines.toIndexedSeq, name, level)
                    content.close
                }
                else {
                    val content = Source.fromFile(path+"test\\"+file)
                    tests += new Test(content.getLines.toIndexedSeq, name, level)
                    content.close
                }
            }
            else settings += ( x.split("=")(0) -> x.split("=")(1) )
        })
        sets.close

        for ( (k,v) <- elevi ) utilizatori += ( k -> v.parola)
        for ( (k,v) <- profesori ) utilizatori += ( k -> v.parola)
    }

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
    class Lectura(sourceTest: IndexedSeq[String], val name: String, val level: Int){
        val info = (for ( x <- 0 until sourceTest.size ) yield sourceTest(x) + "\n" ).mkString
    }
    
    class Elev(val parola: String, val nume: String, val prenume: String, val scoala: String, val clasa: String, val nivel: Int){}
    class Profesor(val parola: String, val nume: String, val prenume: String, val scoala: String, val disciplina: String){}

    def getInfo: Set[Lectura] = info
    def getTests: Set[Test] = tests
    def getSettings(arg: String): String = settings(arg)
    def getLog(arg: String): String = {
        try { utilizatori(arg) }
        catch { case _: Throwable => "" }
    }

    def registerElev(username: String, pass: String, nume: String, prenume: String, scoala: String, clasa: String): Unit = {
        elevi += ( username -> new Elev(pass, nume, prenume, scoala, clasa, 1))
        utilizatori += ( username -> pass )
        val fw = new FileWriter(System.getProperty("user.home") + "\\Draconis\\users.txt", true)
        try { fw.write("\n" + username + "{" + pass +","+ nume +","+ prenume +","+ scoala +","+ clasa +","+ 1) }
        finally fw.close
    }
    def registerProf(username: String, pass: String, nume: String, prenume: String, scoala: String, disciplina: String): Unit = {
        profesori += ( username -> new Profesor(pass, nume, prenume, scoala, disciplina) )
        utilizatori += ( username -> pass )
        val fw = new FileWriter(System.getProperty("user.home") + "\\Draconis\\users.txt", true)
        try { fw.write("\n" + username + "[" + pass +","+ nume +","+ prenume +","+ scoala +","+ disciplina) }
        finally fw.close
    }
    
}