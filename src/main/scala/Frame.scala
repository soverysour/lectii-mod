import scala.swing._

object Frame {

    var login: loginFrame = null
    var mainFrame: principalFrame = null
    var (user, level) = ("", -1)

    def main(args: Array[String]): Unit = {
        Loader.init
        login = new loginFrame
    }

    def commence: Unit = {
        login.close
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
                contents += Button("Register"){ register(usernameField.text, passwordField.password.mkString) }
            }
            border = Swing.EmptyBorder(10, 10, 10, 10)
        }
        centerOnScreen
        visible = true
        
        private[this] def login(name: String, pass: String): Unit = {
            if ( good(name, pass) ){
                if ( Loader.getUsers(name) == ("", -1) )
                    Dialog.showMessage(contents.head, "User does not exist.", title="Authentication Error")
                else {
                    if ( pass == Loader.getUsers(name)._1 ){
                        user = name
                        level = Loader.getUsers(name)._2
                        commence
                    }
                    else Dialog.showMessage(contents.head, "Incorrect password.", title="Authentication Error")
                }
            }
        }
        private[this] def register(name: String, pass: String): Unit = {
            if ( good(name, pass) ) {
                if ( Loader.getUsers(name) == ("", -1) ){
                    Loader.register(name, pass)
                    login(name, pass)
                }
                else Dialog.showMessage(contents.head, "User already exists.", title="Registration Error")
            }
        }
        private[this] def good(name: String, pass: String): Boolean = {
            val alfa = name.replaceAll("[^a-zA-Z0-9]", "")
            val beta = pass.replaceAll("[^a-zA-Z0-9]", "")

            if ( alfa == "" || beta == "" ) {
                Dialog.showMessage(contents.head, "Invalid username and / or password.", title="Input Error")
                false
            }
            else true 
        }
    }

}