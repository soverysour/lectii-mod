package Draconis.romana

import scala.swing._
import scala.swing.event._
import scala.util.Random
import java.awt.{ Color, BasicStroke, Font, RenderingHints }
import java.lang.Error
import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
import scala.collection.mutable.{ Set, ListBuffer }
import Defaults.Frame._
import Defaults.Names._
import Defaults.Misc.m_isVerified
import Defaults.ProcessTestEntries._

object Frame {
  private[this] var loginFr: LoginFrame = _
  private[this] var registerFr: RegisterFrame = _
  private[this] var moduleFr: ModuleFrame = _
  private[this] var elevFr: StudentFrame = _
  private[this] var materialFr: MaterialFrame = _
  private[this] var testFr: TestFrame = _
  private[this] var reviewFr: ReviewFrame = _
  private[this] var instanceFr: InstanceFrame = _
  private[this] var takenTestFr: TakenTestFrame = _

  private[this] def commence(label: String): Unit = {
    loginFr.close
    if (label == studentMark) moduleFr = new ModuleFrame
    else throw new Error("Conturile tip profesor nu au functionalitate, inca.")
  }

  def initial(): Unit = loginFr = new LoginFrame
  def refreshElev(): Unit = if ( elevFr != null ) elevFr.refresh

  private[this] def xyAlign(s: scala.collection.mutable.Buffer[Component]): Unit = s.foreach( x => {x.xLayoutAlignment = 0; x.yLayoutAlignment = 0})
  private[this] def restrictHeight(s: Component): Unit = s.maximumSize = new Dimension(Short.MaxValue, s.preferredSize.height)
  private[this] def restrictWidth(s: Component): Unit = s.maximumSize = new Dimension(s.preferredSize.height, Short.MaxValue)
  private[this] def restrictSize(s: Component): Unit = s.maximumSize = new Dimension(s.preferredSize.width, s.preferredSize.height)

  private[this] val logsFont = new Font(Font.MONOSPACED, Font.BOLD, 14)
  private[this] val menuFont = new Font(Font.MONOSPACED, Font.BOLD, 15)
  private[this] val moduleFont = new Font(Font.MONOSPACED, Font.PLAIN, 18)
  private[this] val headerFont = new Font(Font.MONOSPACED, Font.PLAIN, 28)
  private[this] val componentFont = new Font(Font.MONOSPACED, Font.PLAIN, 15)
  private[this] val endFont = new Font(Font.MONOSPACED, Font.BOLD, 16)

  class ModuleFrame extends MainFrame {
    title = m_title
    resizable = true

    private[this] var modules: Set[Button] = Set[Button]()
    Holder.getModules.foreach(x => modules += Button(x){ sweep(x) } )
    modules.foreach(x => {
      restrictHeight(x)
      x.font = moduleFont
    })
    var ss = new Dimension(200, 0)
    val mod = modules.toList

    contents = new BoxPanel(Orientation.Vertical) {
      for ( x <- 0 until mod.size ){
        contents += mod(x)
        if ( x != mod.size - 1 ) contents += Swing.VStrut(10)
        if (mod(x).preferredSize.width > ss.width)
          ss = new Dimension(mod(x).preferredSize.width, ss.height + mod(x).preferredSize.height + 10)
        else ss = new Dimension(ss.width, ss.height + mod(x).preferredSize.height + 10 )
      }
      preferredSize = new Dimension(ss.width + 30, ss.height + 15)
      background = Color.white
      border = Swing.EmptyBorder(15, 15, 15, 15)
    }

    private[this] def sweep(a: String): Unit = {
      close
      elevFr = new StudentFrame(a)
    }

    centerOnScreen
    visible = true
  }

  class TakenTestFrame(test: String, discr: String) extends MainFrame {
    title = test
    resizable = true
    preferredSize = new Dimension(800, 600)

    private[this] def processEntry(s: String): List[Component] = {
      if ( t_getType(s) == completeSpace || t_getType(s) == dragDrop ){
        if ( t_hasSolution(s) ) List(new Label(t_getName(s) + "  -->  " + t_getSolution(s)) { font = componentFont })
        else List()
      }
      else {
        val ask = new Label(t_getName(s)) { font = componentFont; background = Color.white }
        var something = List[CheckBox]()
        t_splitAns(t_getSolution(s)).foreach( x => {
          val y = if (t_isSelected(x)) t_getOption(x) else x
            something = new CheckBox(y){ font = componentFont; enabled = false; selected = y != x; background = Color.white} :: something
        })
        ask :: something
      }
    }

    val (good, bad) = Core.getResults(test.split("[|]")(0).trim, discr).partition(m_isVerified(_))

    contents = new ScrollPane {
      contents = new BoxPanel(Orientation.Vertical){
        contents += new Label(tt_bad){ font = headerFont }
        contents += Swing.VStrut(15)

        bad.foreach( x => {
          val returned = processEntry(x)
          if ( returned.size > 1 ){
            contents += returned(0)
            val box = new BoxPanel(Orientation.Horizontal)
            for ( ss <- 1 until returned.size ){
              box.contents += returned(ss)
              box.contents += Swing.HStrut(5)
            }
            restrictSize(box)
            box.background = Color.white
            contents += box
            contents += Swing.VStrut(15)
          }
          else if ( returned.size > 0 ){
            contents += returned(0)
            contents += Swing.VStrut(15)
          }
        })
        contents += Swing.VStrut(15)
        contents += new Label(tt_good){ font = headerFont }
        contents += Swing.VStrut(15)
        good.foreach( x => {
          val returned = processEntry(x)
          if ( returned.size > 1 ){
            contents += returned(0)
            val box = new BoxPanel(Orientation.Horizontal)
            for ( ss <- 1 until returned.size ){
              box.contents += returned(ss)
              box.contents += Swing.HStrut(5)
            }
            restrictSize(box)
            box.background = Color.white
            contents += box
            contents += Swing.VStrut(15)
          }
          else if ( returned.size > 0 ){
            contents += returned(0)
            contents += Swing.VStrut(15)
          }
        })

        xyAlign(contents)
        background = Color.white
        border = Swing.EmptyBorder(15, 15, 15, 15)
      }
      background = Color.white
    }

    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
    override def closeOperation = {
      close
      elevFr.visible = true
    }

    centerOnScreen
    visible = true
  }

  class InstanceFrame(n: String) extends MainFrame {
    title = if_title
    resizable = true
    preferredSize = new Dimension(500, 500)

    contents = new ScrollPane {
      contents = new BoxPanel(Orientation.Vertical) {
        val instances = Core.testInstances(n)
        for ( x <- 0 until instances.size ){
          val button = Button(instances(x)._1){ goTo(instances(x)._1, instances(x)._2) }
          button.font = componentFont
          contents += button
          if ( x < instances.size - 1 ) contents += Swing.VStrut(5)
        }
        background = Color.white
        border = Swing.EmptyBorder(15, 15, 15, 15)
      }
      background = Color.white
    }

    private[this] def goTo(test: String, discr: String): Unit = {
      close
      takenTestFr = new TakenTestFrame(test, discr)
    }

    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
    override def closeOperation = {
      close
      elevFr.visible = true
    }

    centerOnScreen
    visible = true
  }

  class ReviewFrame extends MainFrame {
    title = rf_title
    resizable = true
    preferredSize = new Dimension(500, 500)

    contents = new ScrollPane{
      contents = new BoxPanel(Orientation.Vertical) {
        val instances = Holder.getTests
        for ( x <- 0 until instances.size ){
          val button = Button(instances(x).name){ sweep(instances(x).name) }
          button.font = componentFont
          contents += button
          if ( x < instances.size - 1 ) contents += Swing.VStrut(5)
        }
        background = Color.white
        border = Swing.EmptyBorder(15, 15, 15, 15)
      }
      background = Color.white
    }

    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
    override def closeOperation = {
      close
      elevFr.visible = true
    }
    private[this] def sweep(name: String): Unit = {
      close
      instanceFr = new InstanceFrame(name)
    }

    centerOnScreen
    visible = true
  }

  class StudentFrame(a: String) extends MainFrame {
    Core.initModule(a)

    title = a
    resizable = true
    preferredSize = new Dimension(800, 300)

    private[this] def formatAverage: String = s"Nota medie a dumneavoastra este ${Holder.getStats._2}."
    private[this] def formatPercentage: String = s"Lectia curenta este ${Holder.getStats._1}% completa."
    private[this] var average = new Label(formatAverage){ font = menuFont }
    private[this] var percentage = new Label(formatPercentage){ font = menuFont }

    def refresh(): Unit = {
      percentage.text = formatPercentage
      average.text = formatAverage
    }

    contents = new TabbedPane() {
      pages += new TabbedPane.Page(s_materialTab, new ScrollPane{
        contents = new FlowPanel {
          Holder.getInfo.foreach(x => {
            val button = Button(x.name) { open(materialName, x.name) }
            button.font = menuFont
            contents += button
          })
          border = Swing.EmptyBorder(15, 15, 15, 15)
          background = Color.white
        }
      }){font = menuFont}
      pages += new TabbedPane.Page(s_testTab, new ScrollPane{
        contents = new FlowPanel {
          Holder.getTests.foreach(x => {
            val button = Button(x.name) { open(testName, x.name) }
            button.font = menuFont
            contents += button
          })
          border = Swing.EmptyBorder(15, 15, 15, 15)
          background = Color.white
        }
      }){font = menuFont}
      pages += new TabbedPane.Page(s_profileTab, new BoxPanel(Orientation.Horizontal) {
        contents += new BoxPanel(Orientation.Vertical){
          contents += Swing.VStrut(10)
          contents += new Label(s"${s_profileUser}: ${Holder.getUser.username}"){ font = menuFont }
          contents += Swing.VStrut(10)
          contents += new Label(s"${s_profileSurname}: ${Holder.getUser.surname}"){ font = menuFont }
          contents += Swing.VStrut(10)
          contents += new Label(s"${s_profileName}: ${Holder.getUser.name}"){ font = menuFont }
          contents += Swing.VStrut(10)
          contents += new Label(s"${s_profileSchool}: ${Holder.getUser.school}"){ font = menuFont }
          contents += Swing.VStrut(10)
          contents += new Label(s"${s_profileClass}: ${Holder.getUser.optText}"){ font = menuFont }
          contents += Swing.VStrut(10)
          xLayoutAlignment = 0
          yLayoutAlignment = 0
          background = Color.white
          border = Swing.EmptyBorder(15, 15, 15, 15)
        }
        contents += new BoxPanel(Orientation.Vertical){
          contents += Swing.VStrut(10)
          contents += percentage
          contents += Swing.VStrut(10)
          contents += average
          contents += Swing.VStrut(10)
          var button = Button(s_pastTests){ changeToReview }
          button.font = menuFont
          contents += button
          contents += Swing.VStrut(10)
          button = Button(s_changeModule){
            close
            moduleFr = new ModuleFrame
          }
          button.font = menuFont
          contents += button
          contents += Swing.VStrut(10)
          xLayoutAlignment = 0
          yLayoutAlignment = 0
          background = Color.white
          border = Swing.EmptyBorder(15, 15, 15, 15)
        }
        border = Swing.EmptyBorder(15, 15, 15, 15)
        background = Color.white
      }){font = menuFont}
    }

    centerOnScreen
    visible = true

    private[this] def changeToReview(): Unit = {
      visible = false
      reviewFr = new ReviewFrame
    }
    private[this] def open(label: String, identity: String): Unit = {
      if (label == materialName){
          elevFr.visible = false
          materialFr = new MaterialFrame(Holder.getExactInfo(identity))
      }
      else if (label == testName){
          elevFr.visible = false
          testFr = new TestFrame(Holder.getExactTest(identity))
      }
    }
  }

  class TestFrame(source: Holder.Test) extends MainFrame {
    title = source.name
    resizable = true
    preferredSize = new Dimension(800, 600)

    private[this] var emptySpaces = List[(String, TextField)]()
    private[this] var checkBoxes = ListBuffer[(String, List[CheckBox])]()
    private[this] var leftToRight = ListBuffer[(ToggleButton, ToggleButton)]()

    contents = new ScrollPane {
      contents = new BoxPanel(Orientation.Vertical) {
        for (problem <- source.problems) {
          if (problem.kind == completeSpace) {
            contents += Swing.VStrut(10)

            for (workSample <- problem.workload) {
              contents += new Label(workSample._1){font = menuFont}
              contents += Swing.VStrut(5)
              emptySpaces = workSample._1 -> new TextField{columns = 20; background = Color.white; foreground = Color.black; font = menuFont} :: emptySpaces

              restrictSize(emptySpaces(0)._2)

              contents += emptySpaces(0)._2
              contents += Swing.VStrut(5)
            }
            contents += Swing.VStrut(10)
          } else if (problem.kind == chooseVariant) {
            contents += Swing.VStrut(10)

            for (workSample <- problem.workload) {
              contents += new Label(workSample._1){font = menuFont}
              contents += Swing.VStrut(5)

              contents += new BoxPanel(Orientation.Horizontal) {
                checkBoxes.+=:(workSample._1 -> List())
                for (variant <- t_splitAns(workSample._2)) {
                  if (t_isSelected(variant)) {
                    checkBoxes(0) = checkBoxes(0)._1 -> (new CheckBox(t_getOption(variant)){font = menuFont; background = Color.white; foreground = Color.black} :: checkBoxes(0)._2)
                    contents += checkBoxes(0)._2(0)
                    contents += Swing.HStrut(5)
                  } else {
                    checkBoxes(0) = checkBoxes(0)._1 -> (new CheckBox(variant){font = menuFont; background = Color.white; foreground = Color.black} :: checkBoxes(0)._2)
                    contents += checkBoxes(0)._2(0)
                    contents += Swing.HStrut(5)
                  }
                }
                background = Color.white
              }
              contents += Swing.VStrut(5)
            }
            contents += Swing.VStrut(10)
          } else if (problem.kind == dragDrop) {
            contents += Swing.VStrut(10)

            var dragDrops = List[(ToggleButton, ToggleButton)]()

            for (workSample <- problem.workload) dragDrops = new ToggleButton(workSample._1){font = menuFont} -> new ToggleButton(workSample._2){font = menuFont} :: dragDrops
            val left = Random.shuffle(dragDrops.map(x => x._1).filter(x => x.text != nonExisting))
            val right = Random.shuffle(dragDrops.map(x => x._2).filter(x => x.text != nonExisting))

            def checkOut(left: ToggleButton, right: ToggleButton): Unit = {
              leftToRight = leftToRight.filter(x => x._1 != left && x._2 != right)
            }

            val leftPanel = new BoxPanel(Orientation.Vertical) {
              contents += Swing.VStrut(5)
              left.foreach(x => {
                contents += x
                contents += Swing.VStrut(5)
                listenTo(x)
              })
              background = Color.white
              border = Swing.EmptyBorder(5, 5, 5, 5)
            }
            restrictSize(leftPanel)

            val rightPanel = new BoxPanel(Orientation.Vertical) {
              contents += Swing.VStrut(5)
              right.foreach(x => {
                contents += x
                contents += Swing.VStrut(5)
                listenTo(x)
              })
              background = Color.white
              border = Swing.EmptyBorder(5, 5, 5, 5)
            }
            restrictSize(rightPanel)

            if (leftPanel.preferredSize.height > rightPanel.preferredSize.height)
              rightPanel.preferredSize = leftPanel.preferredSize
            else leftPanel.preferredSize = rightPanel.preferredSize

            val drawPanel = new Panel {
              preferredSize = {
                if (leftPanel.preferredSize.height > rightPanel.preferredSize.height)
                  new Dimension(200, leftPanel.preferredSize.height)
                else new Dimension(200, rightPanel.preferredSize.height)
              }

              override def paintComponent(g: Graphics2D): Unit = {
                g.clearRect(0, 0, size.width, size.height)
                g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
                g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

                g.setColor(Color.white)
                g.fillRect(0, 0, size.width, size.height)

                val (isLeft, isRight) = if ( left.size > right.size ) (0, 1) else (1, 0)
                val addon = (if (isLeft == 1) left(0).locationOnScreen.getY - right(0).locationOnScreen.getY else left(0).locationOnScreen.getY - right(0).locationOnScreen.getY).toInt

                g.setColor(Color.black)
                left.foreach(x => g.fillOval(0, x.location.getY.toInt + addon * isLeft, 12, 12))
                right.foreach(x => g.fillOval(180, x.location.getY.toInt + addon * isRight, 12, 12))

                g.setStroke(new BasicStroke(4))
                leftToRight.foreach(x => {
                  g.drawLine(6, x._1.location.getY.toInt + 6 + addon * isLeft, 186, x._2.location.getY.toInt + 6 + addon * isRight)
                })
              }
            }
            restrictSize(drawPanel)

            rightPanel.reactions += {
              case ButtonClicked(b) => b match {
                case button: ToggleButton => {
                  if (button.selected) {
                    right.foreach(x => {
                      if (x != button)
                        x.selected = false
                    })
                    left.foreach(y => {
                      if (y.selected) {
                        checkOut(y, button)
                        leftToRight += (y -> button)
                        y.selected = false
                        button.selected = false
                      }
                    })
                  }
                  drawPanel.repaint
                }
              }
              case _ => {}
            }
            leftPanel.reactions += {
              case ButtonClicked(b) => b match {
                case button: ToggleButton => {
                  if (button.selected) {
                    left.foreach(x => {
                      if (x != button)
                        x.selected = false
                    })
                    right.foreach(y => {
                      if (y.selected) {
                        checkOut(button, y)
                        leftToRight += (button -> y)
                        y.selected = false
                        button.selected = false
                      }
                    })
                  }
                  drawPanel.repaint
                }
              }
              case _ => {}
            }

            contents += new BoxPanel(Orientation.Horizontal) {
              contents += leftPanel
              contents += Swing.HStrut(15)
              contents += drawPanel
              contents += Swing.HStrut(15)
              contents += rightPanel

              background = Color.white
            }
          }
        }
        xyAlign(contents)

        contents += Swing.VStrut(15)
        val button = Button(t_finishButton) { closeOperation }
        button.font = endFont
        contents += button
        contents.foreach(_ match {
          case x: BoxPanel => restrictSize(x)
          case _           => {}
        })

        background = Color.white
        border = Swing.EmptyBorder(10, 10, 10, 10)
      }
    }

    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
    override def closeOperation = {
      val b = Dialog.showOptions(
        contents.head,
        t_closeDialogMessage,
        t_closeDialogTitle,
        entries = t_closeDialogEntries,
        initial = 1
      )

      if (b == Dialog.Result.Ok) {
        val newSpaces = emptySpaces.map(x => x._1 -> x._2.text)
        var newChecks = List[(String, List[(String, Boolean)])]()
        checkBoxes.foreach(x => {
          newChecks = (x._1 -> x._2.map(y => (y.text, y.selected))) :: newChecks
        })
        val leftRights = leftToRight.map(x => (x._1.text, x._2.text)).toList

        close
        elevFr.visible = true
        Core.evaluate(newSpaces, newChecks, leftRights, title)
      }
    }

    centerOnScreen
    visible = true
  }

  class MaterialFrame(source: Holder.Material) extends MainFrame {
    title = source.name
    resizable = false
    preferredSize = new Dimension(640, 480)

    contents = new ScrollPane {
      contents = new EditorPane("text/html", source.info) {
        background = Color.white
        foreground = Color.black

        editable = false
      }
    }

    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
    override def closeOperation = {
      close
      elevFr.visible = true
    }

    centerOnScreen
    visible = true
  }

  class RegisterFrame extends MainFrame {
    title = r_title
    preferredSize = new Dimension(220, 290)
    resizable = false

    private[this] val cUsername = new TextField
    private[this] val cPassword = new PasswordField
    private[this] val cSurname = new TextField
    private[this] val cName = new TextField
    private[this] val cSchool = new TextField
    private[this] val cClassLabel = new Label(r_optionalLabelStudent)
    private[this] val cClass = new TextField
    private[this] val cCreation = Button(r_creationButtonLabel) {
      register(cUsername.text, cPassword.password.mkString, cSurname.text,
        cName.text, cSchool.text, cClass.text)
    }
    private[this] val cQuit = Button(r_quitButtonLabel) { back }

    restrictHeight(cUsername)
    restrictHeight(cPassword)
    restrictHeight(cSurname)
    restrictHeight(cName)
    restrictHeight(cSchool)
    restrictHeight(cClass)

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label(r_usernameLabel)
        contents += Swing.HStrut(5)
        contents += cUsername
      }
      contents += Swing.VStrut(5)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label(r_passwordLabel)
        contents += Swing.HStrut(5)
        contents += cPassword
      }
      contents += Swing.VStrut(5)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label(r_surnameLabel)
        contents += Swing.HStrut(5)
        contents += cSurname
      }
      contents += Swing.VStrut(5)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label(r_nameLabel)
        contents += Swing.HStrut(5)
        contents += cName
      }
      contents += Swing.VStrut(5)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label(r_schoolLabel)
        contents += Swing.HStrut(5)
        contents += cSchool
      }
      contents += Swing.VStrut(5)
      contents += new BoxPanel(Orientation.Horizontal){
        contents += cClassLabel
        contents == Swing.HStrut(5)
        contents += cClass
      }
      contents += Swing.VStrut(5)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += cQuit
        contents += Swing.HGlue
        contents += cCreation
      }

      border = Swing.EmptyBorder(10, 10, 10, 10)
      xyAlign(contents)
    }

    centerOnScreen
    visible = true

    private[this] def register(username: String, pass: String, surname: String,
      name: String, school: String, opttext: String): Unit = {
      if (good(List(username, pass, surname, name, school, opttext))) {
        if (Holder.getLog(username) == "") {
          Core.register(username, pass, surname, name, school, opttext)
          back
        } else Dialog.showMessage(contents.head, r_userConflictMessage, title = r_invalidData)
      } else Dialog.showMessage(contents.head, r_invalidInputMessage, title = r_invalidData)
    }

    private[this] def good(arg: List[String]): Boolean = {
      for (x <- arg) if (x == "" || x.replaceAll(r_unallowedChars, "") != x) return false
      true
    }
    private[this] def back(): Unit = {
      close
      loginFr.visible = true
    }
    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
    override def closeOperation = { back }
  }

  class LoginFrame extends MainFrame {
    title = l_title
    preferredSize = new Dimension(300, 150)
    resizable = false

    private[this] val usernameField = new TextField { font = logsFont }
    private[this] val passwordField = new PasswordField { font = logsFont }
    restrictHeight(usernameField)
    restrictHeight(passwordField)

    contents = new BoxPanel(Orientation.Vertical){
      contents += new BoxPanel(Orientation.Horizontal){
        contents += new BoxPanel(Orientation.Vertical) {
          contents += new Label(l_usernameLabel){ font = logsFont }
          contents += Swing.HStrut(15)
          contents += new Label(l_passwordLabel){ font = logsFont }
        }
        contents += Swing.VStrut(5)
        contents += new BoxPanel(Orientation.Vertical) {
          contents += usernameField
          contents += Swing.HStrut(15)
          contents += passwordField
        }
      }
      contents += Swing.VStrut(30)
      contents += new BoxPanel(Orientation.Horizontal){
        val button = Button(l_loginButton) { login(usernameField.text, passwordField.password.mkString) }
        button.font = logsFont
        contents += button
        if ( Holder.getType == studentMark ){
          contents += Swing.HGlue
          val button = Button(l_registerButton) { register }
          button.font = logsFont
          contents += button
        }
      }
      xyAlign(contents)
      border = Swing.EmptyBorder(15, 15, 15, 15)
    }

    centerOnScreen
    visible = true

    private[this] def login(name: String, pass: String): Unit = {
      if ( Holder.getType == professorMark && name == "" && pass == lId )
        commence(professorMark)
      else if (Holder.getLog(name) == "")
        Dialog.showMessage(contents.head, l_noUserMessage, title = l_invalidData)
      else if (pass == Holder.getLog(name)) {
        Holder.setUser(name)
        commence(studentMark)
      } else Dialog.showMessage(contents.head, l_badPasswordMessage, title = l_invalidData)
    }
    private[this] def register(): Unit = {
      visible = false
      registerFr = new RegisterFrame
    }
  }
}
