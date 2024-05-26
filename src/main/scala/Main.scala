import java.io.File
import scala.swing._
import scala.swing.event._
import scala.util.Random

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
        showGameOptions(8, 8, 10, "Beginner")
      case ButtonClicked(`intermediateButton`) =>
        showGameOptions(16, 16, 40, "Intermediate")
      case ButtonClicked(`expertButton`) =>
        showGameOptions(30, 16, 99, "Expert")
    }

    private def showGameOptions(width: Int, height: Int, numMines: Int, difficulty: String): Unit = {
      val options = Seq("Start game", "Load level", "Random level", "Level builder")
      val dialog = new Dialog {
        title = "Game Options"
        modal = true
        val optionList = new ListView(options)
        optionList.selection.intervalMode = ListView.IntervalMode.Single
        contents = new BoxPanel(Orientation.Vertical) {
          contents += new ScrollPane(optionList)
          contents += Button("Select") {
            close()
          }
          border = Swing.EmptyBorder(10, 10, 10, 10)
        }
        centerOnScreen()
        open()
        optionList.selection.indices.headOption
      }

      dialog.optionList.selection.indices.headOption match {
        case Some(0) => startGame(width, height, numMines, difficulty)
        case Some(1) => loadGame(width, height, numMines, difficulty)
        case Some(2) => startRandomLevel(width, height, numMines, difficulty)
        case Some(3) => openLevelBuilder(width, height, numMines, difficulty)
        case _ => println("No option selected")
      }
    }

    private def startRandomLevel(width: Int, height: Int, numMines: Int, difficulty: String): Unit = {
      val dir = new File(s"./temp/$difficulty")
      if (dir.exists && dir.isDirectory) {
        val files = dir.listFiles.filter(_.isFile).toList
        if (files.nonEmpty) {
          val randomFile = files(Random.nextInt(files.length))
          val savedBoard = new Board(width, height, numMines, difficulty)
          savedBoard.loadLevel(randomFile.getPath)

          val mainFrame = new MainFrame {
            title = "Scala Minesweeper"
            val ui = new UI(width, height, numMines, difficulty)
            ui.setBoard(savedBoard)
            contents = ui
            size = new Dimension(1600, 800)
            centerOnScreen()
            visible = true
          }
          mainFrame.visible = true
          this.visible = false
        } else {
          println(s"No level files found in ./temp/$difficulty")
        }
      } else {
        println(s"Directory ./temp/$difficulty does not exist")
      }
    }
    private def loadGame(width: Int, height: Int, numMines: Int, difficulty: String): Unit = {
      val chooser = new FileChooser(new File(s"./temp/${difficulty}"))
      chooser.title = "Choose a Level File"
      val result = chooser.showOpenDialog(contents.head)
      if (result == FileChooser.Result.Approve) {
        val file = chooser.selectedFile
        val savedBoard = new Board(width, height, numMines, difficulty)
        savedBoard.loadLevel(file.getPath)

        val mainFrame = new MainFrame {
          title = "Scala Minesweeper"
          val ui = new UI(width, height, numMines, difficulty)
          ui.setBoard(savedBoard)
          contents = ui
          size = new Dimension(1600, 800)
          centerOnScreen()
          visible = true
        }
        mainFrame.visible = true
        this.visible = false
      }
    }
    private def startGame(width: Int, height: Int, numMines: Int, difficulty: String): Unit = {
      val mainFrame = new MainFrame {
        title = "Scala Minesweeper"
        contents = new UI(width, height, numMines, difficulty)
        size = new Dimension(1600, 800)
        centerOnScreen()
        visible = true
      }
      mainFrame.visible = true
      this.visible = false
    }

    private def openLevelBuilder(width: Int, height: Int, numMines: Int, difficulty: String): Unit = {
      val chooser = new FileChooser(new File(s"./temp/${difficulty}"))
      chooser.title = "Choose level file to edit"
      val result = chooser.showOpenDialog(contents.head)
      if(result == FileChooser.Result.Approve){
        val file = chooser.selectedFile
        val savedBoard = new Board(width, height, numMines, difficulty, false)
        savedBoard.loadLevel(file.getPath)

        val mainFrame = new MainFrame{
          title = "Level builder"
          val ui = new UI(width, height, numMines, difficulty) {
            setBoard(savedBoard)
          }
          val levelBuilder = new LevelBuilder(savedBoard, difficulty, file.getPath, ui)
          contents = new BorderPanel {
            layout(levelBuilder) = BorderPanel.Position.North
            layout(ui) = BorderPanel.Position.Center
          }
          size = new Dimension(1600, 800)
          centerOnScreen()
          visible = true
        }
        mainFrame.visible = true
        this.visible = false
      }
    }
  }
}
