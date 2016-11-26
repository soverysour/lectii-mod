import scala.swing._
import scala.swing.event._

object Frame {

    var loginFr: loginFrame = null
    var mainFrame: principalFrame = null
    var accountFrame: registerFrame = null
    var currentUser: String = ""

    def main(args: Array[String]): Unit = {
        Loader.init
        loginFr = new loginFrame
    }

    def commence: Unit = {
        loginFr.close
        mainFrame = new principalFrame
    }

    private[this] def restrictHeight(s: Component): Unit = {
        s.maximumSize = new Dimension(Short.MaxValue, s.preferredSize.height)
    }

    class principalFrame extends MainFrame {
        title = Loader.getSettings("titlu")
        preferredSize = new Dimension(640, 480)
        resizable = false

        contents = new BoxPanel( Orientation.Vertical ){}

        centerOnScreen
        visible = true
    }

    class registerFrame extends MainFrame {
        title = "Register"
        preferredSize = new Dimension(220, 290)
        resizable = false

        val username = new TextField
        val password = new PasswordField
        val nume = new TextField
        val prenume = new TextField
        val scoala = new TextField
        val optional = new Label("Clasa")
        val opttext = new TextField
        val statelev = new RadioButton("Elev")
        val statprof = new RadioButton("Profesor")
        val status = new ButtonGroup(statelev, statprof)
        val creare = Button("Creare"){register(username.text, password.password.mkString, nume.text, prenume.text, scoala.text, opttext.text, statelev.selected)}
        val quit = Button("Inapoi"){close; loginFr.visible = true}

        restrictHeight(username)
        restrictHeight(password)
        restrictHeight(nume)
        restrictHeight(prenume)
        restrictHeight(scoala)
        restrictHeight(opttext)
        statelev.selected = true

        contents = new BoxPanel( Orientation.Vertical ){
            contents += new BoxPanel( Orientation.Horizontal ){
                contents += new Label("Username")
                contents += Swing.HStrut(5)
                contents += username
            }
            contents += Swing.VStrut(5)
            contents += new BoxPanel( Orientation.Horizontal ){
                contents += new Label("Password")
                contents += Swing.HStrut(5)
                contents += password
            }
            contents += Swing.VStrut(5)
            contents += new BoxPanel( Orientation.Horizontal ){
                contents += new Label("Nume")
                contents += Swing.HStrut(5)
                contents += nume
            }
            contents += Swing.VStrut(5)
            contents += new BoxPanel( Orientation.Horizontal ){
                contents += new Label("Prenume")
                contents += Swing.HStrut(5)
                contents += prenume
            }
            contents += Swing.VStrut(5)
            contents += new BoxPanel( Orientation.Horizontal ){
                contents += new Label("Scoala")
                contents += Swing.HStrut(5)
                contents += scoala
            }
            contents += Swing.VStrut(5)
            contents += new BoxPanel( Orientation.Horizontal ){
                contents += Swing.HStrut(5)
                contents += statelev
                contents += Swing.HStrut(10)
                contents += statprof
            }
            contents += Swing.VStrut(5)
            contents += optional
            contents == Swing.VStrut(5)
            contents += opttext
            contents += Swing.VStrut(5)
            contents += new BoxPanel( Orientation.Horizontal ){
                contents += quit
                contents += Swing.HGlue
                contents += creare
            }


            border = Swing.EmptyBorder(10, 10, 10, 10)
            contents.foreach( x => x.xLayoutAlignment = 0.5 )
        }

        listenTo(statelev)
        listenTo(statprof)
        reactions += {
            case ButtonClicked(button) => {
                button.text match {
                    case "Elev" => optional.text = "Clasa"
                    case "Profesor" => optional.text = "Disciplina"
                }
            }
        }

        centerOnScreen
        visible = true

        private[this] def register(username: String, pass: String, nume: String, prenume: String, scoala: String, opttext: String, statelev: Boolean): Unit = {
            if ( good(List(username, pass, nume, prenume, scoala, opttext)) ){
                if ( statelev ) Loader.registerElev(username, pass, nume, prenume, scoala, opttext)
                else Loader.registerProf(username, pass, nume, prenume, scoala, opttext)
                close
                loginFr.visible = true
            }
        }
        
        private[this] def good(arg: List[String]): Boolean = {
            for ( x <- arg ){
                if ( x == "" || x.replaceAll("[^a-zA-Z0-9]", "") != x ){
                    Dialog.showMessage(contents.head, "Invalid input data", title = "Input Error")
                    return false
                }
            }
            true
        }
    }

    class loginFrame extends MainFrame {
        title = "Login or Register"
        preferredSize = new Dimension(280, 150)
        resizable = false

        val usernameField = new TextField
        val passwordField = new PasswordField
        restrictHeight(usernameField)
        restrictHeight(passwordField)
        
        contents = new BoxPanel( Orientation.Vertical ){
            contents += new BoxPanel( Orientation.Horizontal ){
                contents += new Label( "Username" )
                contents += Swing.HStrut(5)
                contents += usernameField
            }
            contents += Swing.VStrut(5)
            contents += new BoxPanel( Orientation.Horizontal ){
                contents += new Label( "Password" )
                contents += Swing.HStrut(5)
                contents += passwordField
            }
            contents += Swing.VStrut(10)
            contents += new BoxPanel( Orientation.Horizontal ){
                contents += Button( "Login" ) { login(usernameField.text, passwordField.password.mkString) }
                contents += Swing.HGlue
                contents += Button("Register"){ register }
            }
            border = Swing.EmptyBorder(10, 10, 10, 10)
        }
        centerOnScreen
        visible = true
        
        private[this] def login(name: String, pass: String): Unit = {
            if ( Loader.getLog(name) == "" )
                Dialog.showMessage(contents.head, "User does not exist.", title = "Authentication Error")
            else if ( pass == Loader.getLog(name) ){
                    currentUser = name
                    commence
            }
            else Dialog.showMessage(contents.head, "Incorrect password.", title = "Authentication Error")
        }
        private[this] def register: Unit = {
            visible = false
            accountFrame = new registerFrame
        }
    }

}