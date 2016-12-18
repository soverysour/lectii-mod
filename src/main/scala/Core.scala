import scala.io.Source
import java.io.{File, FileWriter}

object Core {
	
	//Core initialisation of all users.
	def initUsers: Unit = {
		val profiles = Source.fromFile(System.getProperty("user.home")+"/Draconis/users.txt")
        Holder.loadUsers(profiles.getLines.toIndexedSeq)
        profiles.close
	}
	
	//Core initialisation of a chosen module.
	def initModule(s: String): Unit = {
		Holder.setModule(s)
		
		val genSet = Source.fromFile(System.getProperty("user.home")+"/Draconis/"+s+"/settings.txt")
		
        genSet.getLines.toIndexedSeq.foreach( x => {
            val ss = x.split("[=]")
            readNload(ss(0), ss(1), ss(2))
        })
		
		genSet.close
    }
    
    def register(us: String, pa: String, nu: String, pr: String, sc: String, 
    			 opt: String, isElev: Boolean): Unit = {
    			 
        var sp = if ( isElev ) "e" else "p"
    	val link = us +","+ pa +","+ nu +","+ pr +","+ sc +","+ opt +","+ sp

        Holder.loadUsers(IndexedSeq(link))

        val fw = new FileWriter(System.getProperty("user.home") + "/Draconis/users.txt", true)
        try { fw.write( link + "\n" ) }
        finally fw.close
    }
    
    
    //From the settings file, either adds a setting, a test or a material.
    private[this] def readNload(s1: String, s2: String, s3: String): Unit = {
        if ( s1 == "1" ) Holder.setSettings( s2 -> s3 )
        else {
            val path =
            System.getProperty("user.home")+"/Draconis/"+Holder.getModule
            val (one, two) = (s3.split("[,]")(0), s3.split("[,]")(1).toInt)
            if ( s1 == "2" ){
                val here = Source.fromFile(path+"/material/"+s2)
                Holder.addLectura(here.getLines.toIndexedSeq, one, two)
                here.close
            }
            else if ( s1 == "3" ){
                val here = Source.fromFile(path+"/test/"+s2)
                Holder.addTest(here.getLines.toIndexedSeq, one, two)
                here.close
            }
            else {
                val here = Source.fromFile(path+"/gallery/"+s2)
                Holder.addGallery(one, two)
                here.close
            }
        }
    }
    
    def getModules: List[String] = new File(System.getProperty("user.home")+"/Draconis/").listFiles.
    	filter(_.isDirectory).toList.map(_.getName)
}
