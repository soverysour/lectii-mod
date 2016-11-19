import scala.swing._

object Frame {
    val login = new MainFrame
    val principal = new MainFrame

    def main(args: Array[String]): Unit = {
        Loader.init
        login.title = Loader.getSet("titlu")
        login.preferredSize = new Dimension(320, 240)
        login.contents = new Label("Alfa romeo, bre")
        login.visible = true

    }
}