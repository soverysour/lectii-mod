import scala.swing._
import scala.collection.mutable.{Map, Set}
import scala.io.Source
import java.io.File

object Loader {
    private[this] var users: Map[String, (String, String)] = Map[String, (String, String)]()
    private[this] var settings: Map[String, String] = Map[String, String]()
    private[this] var info: Set[Lectura] = Set[Lectura]()
    private[this] var tests: Set[Test] = Set[Test]()

    def init: Unit = {
        val path = System.getProperty("user.home") + "\\Draconis"

        val recordings = Source.fromFile(path+"\\list.txt").getLines.toIndexedSeq
        recordings.foreach( x => {
            users += ( x.split("[@]")(0) -> (x.split("[@]")(1), x.split("[@]")(2)) )
        })

        val sets = Source.fromFile(path+"\\set.txt").getLines.toIndexedSeq
        sets.foreach( x => settings += ( x.split("=")(0) -> x.split("=")(1) ))

        val files = new File(path+"\\material").listFiles.map(_.getName)
        files.foreach( x => {
            info += new Lectura(Source.fromFile(path+"\\material\\"+x).getLines.toIndexedSeq)
        })


        val testPaths = new File(path+"\\test").listFiles.map(_.getName)
        testPaths.foreach( x => {
            tests += new Test(Source.fromFile(path+"\\test\\"+x).getLines.toIndexedSeq)
        })

        info foreach println
    }

    class Test(sourceTest: IndexedSeq[String]){
        class Exercitiu(val tip: String, val ex: Set[String]){}
        private[this] var subiect = Set[Exercitiu]()
        private[this] var exer = new Exercitiu("#", Set())
        for ( x <- 1 until sourceTest.size ){
            if ( sourceTest(x) == "##" ) subiect += new Exercitiu(exer.tip, exer.ex)
            else if ( sourceTest(x).replaceFirst("[DTSC][#]{2}[DFPE]", "#") == "#" ) 
                exer = new Exercitiu(sourceTest(x), Set())
            else exer = new Exercitiu(exer.tip, exer.ex + sourceTest(x) )
        }

        val problems = subiect
        val level = sourceTest(0).toInt
    }
    class Lectura(sourceTest: IndexedSeq[String]){
        var accumulator = ""
        for ( x <- 2 until sourceTest.size ) accumulator = accumulator + sourceTest(x) + "\n" 
        
        val title = sourceTest(0)
        val level = sourceTest(1).toInt
        val info = accumulator
    }

    def getSet(arg: String): String = settings(arg)
    
}