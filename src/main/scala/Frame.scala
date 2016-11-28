import scala.swing._
import scala.swing.event._
import java.awt.Color
import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE

object Frame {
    var loginFr: loginFrame = null
    var accountFrame: registerFrame = null

    var elevFr: studFrame = null
    var profFr: teacFrame = null

    val lid: String = getPas + "7Dra8" // XDvCSSCvDX7Dra8

    //Load the login frame and initiate the loading of the users and their credentials.
    def main(args: Array[String]): Unit = {
        Loader.init
        loginFr = new loginFrame
    }

    //This is what happens when someone succesfully logs in. 
    private[this] def commence: Unit = {
        loginFr.close
        if ( Loader.getUser.isElev ) elevFr = new studFrame
        else profFr = new teacFrame
    }
    
    //The frame a student account sees after picking the module.
    class studFrame extends MainFrame {
        title = "Choose your module"
        preferredSize = new Dimension(200, 120)
        resizable = false

        contents = new BoxPanel( Orientation.Vertical ){
            background = Color.gray
        }

        centerOnScreen
        visible = true
    }

    //Teacher's frame.
    class teacFrame extends MainFrame {
        title = "Admin"
        preferredSize = new Dimension(800, 600)
        resizable = false

        

        centerOnScreen
        visible = true
    }

    //The registration frame.
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
        val quit = Button("Inapoi"){ back }
        val special = new PasswordField

        restrictHeight(username); restrictHeight(password); restrictHeight(nume); restrictHeight(prenume); restrictHeight(scoala); restrictHeight(opttext)
        statelev.selected = true
        special.editable = false

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
            contents += special
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
                    case "Elev" => {
                        optional.text = "Clasa"
                        special.peer.setText("")
                        special.editable = false
                        opttext.text = ""
                    }
                    case "Profesor" => {
                        optional.text = "Disciplina"
                        special.editable = true
                        opttext.text = ""
                    }
                }
            }
        }

        centerOnScreen
        visible = true

        private[this] def register(username: String, pass: String, nume: String, prenume: String, scoala: String, opttext: String, statelev: Boolean): Unit = {
            if ( good(List(username, pass, nume, prenume, scoala, opttext)) ){
                if ( Loader.getLog(username) == "" ){
                    if ( statelev ){
                        Loader.register(username, pass, nume, prenume, scoala, opttext, true)
                        back
                    }
                    else if ( special.password.mkString == lid ){
                        Loader.register(username, pass, nume, prenume, scoala, opttext, false)
                        back
                    }
                    else Dialog.showMessage(contents.head, "Invalid token.", title = "Input Error")
                }
                else Dialog.showMessage(contents.head, "User already exists.", title = "Input Error")
            }
            else Dialog.showMessage(contents.head, "Invalid input data.", title = "Input Error")
        }
        
        private[this] def good(arg: List[String]): Boolean = {
            for ( x <- arg )
                if ( x == "" || x.replaceAll("[^a-zA-Z0-9]", "") != x ) return false
            true
        }
        private[this] def back: Unit = {
            close
            loginFr.visible = true
        }
        peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
        override def closeOperation = {loginFr.visible = true; close}
    }

    //The login frame.
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
                    Loader.setUser(name)
                    commence
            }
            else Dialog.showMessage(contents.head, "Incorrect password.", title = "Authentication Error")
        }
        private[this] def register: Unit = {
            visible = false
            accountFrame = new registerFrame
        }
    }

    private[this] def getPas: String = {
        val alfa = "XDvC"
        val beta = alfa + "SS"
        beta + alfa.reverse
    }

    private[this] def restrictHeight(s: Component): Unit = s.maximumSize = new Dimension(Short.MaxValue, s.preferredSize.height)

}