import scala.swing._
import scala.swing.event._

object Main extends SimpleSwingApplication {
  def top: Frame = new MainFrame {
    title = "Scala Minesweeper"
    val menu = new Menu
    menu.centerOnScreen()
    menu.visible = true
  }

  class Menu extends MainFrame {
    title = "Minesweeper menu"
    val buttonSize = new Dimension(200, 50)
    val beginnerButton = new Button("Beginner") {
      preferredSize = buttonSize
      maximumSize = buttonSize
      minimumSize = buttonSize
    }
    val intermediateButton = new Button("Intermediate") {
      preferredSize = buttonSize
      maximumSize = buttonSize
      minimumSize = buttonSize
    }
    val expertButton = new Button("Expert") {
      preferredSize = buttonSize
      maximumSize = buttonSize
      minimumSize = buttonSize
    }

    contents = new GridBagPanel {
      val c = new Constraints
      c.fill = GridBagPanel.Fill.Horizontal // fill the grid horizontally
      c.gridx = 0
      c.gridy = 0
      c.insets = new Insets(10, 10, 10, 10) // paddings
      layout(beginnerButton) = c

      c.gridy = 1
      layout(intermediateButton) = c

      c.gridy = 2
      layout(expertButton) = c

      border = Swing.EmptyBorder(30, 30, 30, 30)
    }

    listenTo(beginnerButton, intermediateButton, expertButton)

    reactions += {
      case ButtonClicked(`beginnerButton`) =>
        startGame(8, 8, 10)
      case ButtonClicked(`intermediateButton`) =>
        startGame(16, 16, 40)
      case ButtonClicked(`expertButton`) =>
        startGame(30, 16, 99)
    }

    def startGame(width: Int, height: Int, numMines: Int): Unit = {
      val mainFrame = new MainFrame {
        title = "Scala Minesweeper"
        contents = new UI(width, height, numMines)
        size = new Dimension(1600, 800)
        centerOnScreen()
        visible = true
      }
      mainFrame.visible = true
      this.visible = false
    }
  }
}
