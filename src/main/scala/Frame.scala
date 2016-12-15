import scala.swing._
import scala.swing.event._
import java.awt.Color
import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
import scala.collection.mutable.Set

object Frame {
    private[this] var loginFr: loginFrame = null
    private[this] var accountFrame: registerFrame = null

    private[this] var elevFr: studFrame = null
    private[this] var starFr: elevFrame = null

    private[this] var profFr: teacFrame = null

    private[this] val lid: String = getPas + "7Dra8" // XDvCSSCvDX7Dra8

    //Load the login frame and initiate the loading of the users and their credentials.
    def main(args: Array[String]): Unit = {
        Holder.init
        loginFr = new loginFrame
    }

    //This is what happens when someone succesfully logs in.
    private[this] def commence: Unit = {
        loginFr.close
        if ( Holder.getUser.isElev ) elevFr = new studFrame
        else profFr = new teacFrame
    }

    //The "pick a module" frame.
    class studFrame extends MainFrame {
        title = "Choose your module"
        resizable = false
        private[this] var asdf: Set[Button] = Set[Button]()

        Holder.getModules.foreach( x => {asdf += Button(x){ sweep(x) }} )
        asdf foreach restrictHeight
        var ss = new Dimension(0, 0)

        contents = new BoxPanel( Orientation.Vertical ){
            asdf.foreach( x => {
                contents += x
                contents += Swing.VStrut(10)
                if ( x.preferredSize.width > ss.width )
                    ss = new Dimension(x.preferredSize.width, ss.height + 80 )
                else ss = new Dimension(ss.width, ss.height + 80)
            })
            border = Swing.EmptyBorder(15, 15, 5, 15)
        }

        private[this] def sweep(a: String): Unit = {
            close
            starFr = new elevFrame(a)
        }

        centerOnScreen
        visible = true
    }

    //The frame a student sees after picking a module.
    class elevFrame(a: String) extends MainFrame {
        Holder.load(a)

        title = Holder.getSettings("titlu")
        resizable = false
        preferredSize = new Dimension(400, 300)

        val stuff = new TabbedPane(){
            pages += new TabbedPane.Page("Materiale", new FlowPanel {
                Holder.getInfo.foreach( x => {
                    contents += Button(x.nume){ open(1, x.nume) }
                })
                border = Swing.EmptyBorder(15, 15, 15, 15)
                background = Color.gray
            })
            pages += new TabbedPane.Page("Teste", new FlowPanel {
                Holder.getTests.foreach( x => {
                    contents += Button(x.nume){ open(2, x.nume) }
                })
                border = Swing.EmptyBorder(15, 15, 15, 15)
                background = Color.gray
            })
            pages += new TabbedPane.Page("Galerie", new FlowPanel {
                Holder.getGallery.foreach( x => {
                    contents += Button(x.nume){ open(3, x.nume) }
                })
                border = Swing.EmptyBorder(15, 15, 15, 15)
                background = Color.gray
            })
            pages += new TabbedPane.Page("Profil", new FlowPanel {


                border = Swing.EmptyBorder(15, 15, 15, 15)
                background = Color.gray
            })
        }

        contents = stuff

        centerOnScreen
        visible = true

        private[this] def open(label: Int, identity: String): Unit = {
            if ( label == 1 ) Holder.getExactInfo(identity) match {
                case Some(x) => new matFrame(x); starFr.visible = false
                case None => {}
            }
            else if ( label == 2 ) Holder.getExactTest(identity) match {
                case Some(x) => new tesFrame(x); starFr.visible = false
                case None => {}
            }
            else Holder.getExactGallery(identity) match {
                case Some(x) => new galFrame(x); starFr.visible = false
                case None => {}
            }
        }
    }

    //An opened test.
    class tesFrame(source: Holder.Test) extends MainFrame {
        title = source.nume
        resizable = false
        preferredSize = new Dimension(640, 480)

        contents = new BoxPanel( Orientation.Vertical ){

        }

        peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
        override def closeOperation = {
            close
            starFr.visible = true
        }

        centerOnScreen
        visible = true
    }

    //An opened material.
    class matFrame(source: Holder.Lectura) extends MainFrame {
        title = source.nume
        resizable = false
        preferredSize = new Dimension(640, 480)

        contents = new TextPane(){
            background = Color.gray
            foreground = Color.white

            text = source.nume + "\n\n" + source.info
            editable = false
        }

        peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
        override def closeOperation = {
            close
            starFr.visible = true
        }

        centerOnScreen
        visible = true
    }

    //An opened gallery entry.
    class galFrame(source: Holder.Gallery) extends MainFrame {
        title = source.nume
        resizable = false
        preferredSize = new Dimension(640, 480)

        contents = new TextPane(){}

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

        private[this] val username = new TextField
        private[this] val password = new PasswordField
        private[this] val nume = new TextField
        private[this] val prenume = new TextField
        private[this] val scoala = new TextField
        private[this] val optional = new Label("Clasa")
        private[this] val opttext = new TextField
        private[this] val statelev = new RadioButton("Elev")
        private[this] val statprof = new RadioButton("Profesor")
        private[this] val status = new ButtonGroup(statelev, statprof)
        private[this] val creare = Button("Creare"){
            register(username.text, password.password.mkString, nume.text,
                     prenume.text, scoala.text, opttext.text, statelev.selected)
        }
        private[this] val quit = Button("Inapoi"){ back }
        private[this] val special = new PasswordField

        restrictHeight(username)
        restrictHeight(password)
        restrictHeight(nume)
        restrictHeight(prenume)
        restrictHeight(scoala)
        restrictHeight(opttext)
        restrictHeight(special)

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

        private[this] def register(username: String, pass: String, nume: String, prenume: String,
                                   scoala: String, opttext: String, statelev: Boolean): Unit = {

            if ( good(List(username, pass, nume, prenume, scoala, opttext)) ){
                if ( Holder.getLog(username) == "" ){
                    if ( statelev ){
                        Holder.register(username, pass, nume, prenume, scoala, opttext, true)
                        back
                    }
                    else if ( special.password.mkString == lid ){
                        Holder.register(username, pass, nume, prenume, scoala, opttext, false)
                        back
                    }
                    else Dialog.showMessage(contents.head, "Invalid token.", title = "Input Error")
                }
                else Dialog.showMessage(contents.head, "User already exists.", title = "Input Error")
            }
            else Dialog.showMessage(contents.head, "Invalid input data.", title = "Input Error")
        }

        private[this] def good(arg: List[String]): Boolean = {
            for ( x <- arg ) if ( x == "" || x.replaceAll("[^a-zA-Z0-9]", "") != x ) return false
            true
        }
        private[this] def back: Unit = {
            close
            loginFr.visible = true
        }
        peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
        override def closeOperation = { back }
    }

    //The login frame.
    class loginFrame extends MainFrame {
        title = "Login or Register"
        preferredSize = new Dimension(280, 150)
        resizable = false

        private[this] val usernameField = new TextField
        private[this] val passwordField = new PasswordField
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

        private[this] def login(nume: String, pass: String): Unit = {
            if ( Holder.getLog(nume) == "" )
                Dialog.showMessage(contents.head, "User does not exist.", title = "Authentication Error")
            else if ( pass == Holder.getLog(nume) ){
                    Holder.setUser(nume)
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

    private[this] def restrictHeight(s: Component): Unit = s.maximumSize =
      new Dimension(Short.MaxValue, s.preferredSize.height)

}
