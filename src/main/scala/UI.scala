import scala.swing._
import scala.swing.event._
import java.awt.event.ActionEvent
import java.io.File
import javax.swing.{Timer => SwingTimer}

class UI(width: Int, height: Int, numMines: Int, difficulty: String) extends BorderPanel {
  private val timeLabel = new Label("Time: 0")
  private val scoreLabel = new Label("Score: 0")
  private var board = new Board(width, height, numMines, difficulty)
  val buttonSize = new Dimension(40, 40)
  private val showScoresButton = new Button("Scores")
  private val recommendMoveButton = new Button("Recommend move")
  private val saveLevelButton = new Button("Save level")
  private val loadMovesButton = new Button("Load moves")

  private val buttons = Array.tabulate(height, width) { (row, col) =>
    new Button {
      preferredSize = buttonSize
      maximumSize = buttonSize
      minimumSize = buttonSize
      listenTo(mouse.clicks)
      reactions += {
        case e: MouseClicked =>
          if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON1 && !board.isGameOver && !board.isGameWon) {
            board.revealCell(row, col)
            updateUI()
            checkGameOver()
          } else if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3 && !board.isGameOver && !board.isGameWon) {
            board.getGrid(row)(col).toggleMark()
            updateUI()
          }
      }
    }
  }

  private val gridPanel = new GridPanel(height, width) {
    contents ++= buttons.flatten.toSeq
    preferredSize = new Dimension(width * buttonSize.width, height * buttonSize.height)
  }

  private val statusPanel = new BoxPanel(Orientation.Horizontal) {
    contents += timeLabel
    contents += Swing.HGlue
    contents += recommendMoveButton
    contents += Swing.HGlue
    contents += showScoresButton
    contents += Swing.HGlue
    contents += loadMovesButton
    contents += Swing.HGlue
    contents += saveLevelButton
    contents += Swing.HGlue
    contents += scoreLabel
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }

  layout(statusPanel) = BorderPanel.Position.North
  layout(gridPanel) = BorderPanel.Position.Center

  val timer = new SwingTimer(1000, (_: ActionEvent) => updateTime())
  timer.start()

  listenTo(showScoresButton)
  reactions += {
    case ButtonClicked(`showScoresButton`) =>
      showHighScores()
  }

  listenTo(loadMovesButton)
  reactions += {
    case ButtonClicked(loadMovesButton) =>
      val chooser = new FileChooser(new File("./temp/moves"))
      chooser.title = "Choose a moves file"
      val result = chooser.showOpenDialog(contents.head)
      if (result == FileChooser.Result.Approve) {
        val file = chooser.selectedFile
        board.playTurnsFromFile(file.getPath)
        updateUI()
      }
  }

  listenTo(recommendMoveButton)
  reactions += {
    case ButtonClicked(`recommendMoveButton`) =>
      board.recommendMove() match {
        case Some((row, col)) => {
          println(s"Recommended move: ($row, $col)")
          updateUI()
          checkGameOver()
        }
        case None => println("No safe moves left!")
      }
  }

  listenTo(saveLevelButton)
  reactions += {
    case ButtonClicked(`saveLevelButton`) =>
      board.saveLevel()
  }

  def setBoard(newBoard: Board): Unit = {
    board = newBoard
    updateUI()
  }
  def updateTime(): Unit = {
    timeLabel.text = s"Time: ${board.getElapsedTime}"
    if (board.isGameOver || board.isGameWon) {
      timer.stop()
    }
  }
  def updateUI(): Unit = {
    for (r <- 0 until height; c <- 0 until width) {
      val cell = board.getGrid(r)(c)

      buttons(r)(c).text = if (cell.getRevealed) {
        if (cell.isMine) "#" else cell.getAdjacentMines.toString
      } else if (cell.getMark) {
        "F"
      } else {
        ""
      }
    }
    timeLabel.text = s"Time: ${board.getElapsedTime}"
    val score = board.calculateScore()
    scoreLabel.text = f"Score: $score%.2f"
  }

  private def checkGameOver(): Unit = {
    if (board.isGameWon) {
      Dialog.showMessage(contents.head, "Congratulations! You won!", title="Victory")
    } else if (board.isGameOver) {
      Dialog.showMessage(contents.head, "Game Over! You hit a mine!", title="Game Over")
    }
  }

  private def showHighScores(): Unit = {
    val scores = board.loadScores()
    val message = if (scores.isEmpty) "No scores available." else scores.mkString("\n")
    Dialog.showMessage(contents.head, message, title = "High Scores")
  }
}
