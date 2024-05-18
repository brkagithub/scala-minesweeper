import scala.swing._
import scala.swing.event._
import java.awt.event.ActionEvent
import javax.swing.{Timer => SwingTimer}

class UI(width: Int, height: Int, numMines: Int) extends BorderPanel {
  val timeLabel = new Label("Time: 0")
  val scoreLabel = new Label("Score: 0")
  val board = new Board(width, height, numMines)
  val buttonSize = new Dimension(40, 40)

  val buttons = Array.tabulate(height, width) { (row, col) =>
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

  val gridPanel = new GridPanel(height, width) {
    contents ++= buttons.flatten.toSeq
    preferredSize = new Dimension(width * buttonSize.width, height * buttonSize.height)
  }

  val statusPanel = new BoxPanel(Orientation.Horizontal) {
    contents += timeLabel
    contents += Swing.HGlue
    contents += scoreLabel
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }

  layout(statusPanel) = BorderPanel.Position.North
  layout(gridPanel) = BorderPanel.Position.Center

  val timer = new SwingTimer(1000, (_: ActionEvent) => updateTime())
  timer.start()

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

      println(buttons(r)(c).text)
    }
    timeLabel.text = s"Time: ${board.getElapsedTime}"
    val score = board.calculateScore()
    scoreLabel.text = f"Score: $score%.2f"
  }

  def checkGameOver(): Unit = {
    if (board.isGameWon) {
      Dialog.showMessage(contents.head, "Congratulations! You won!", title="Victory")
    } else if (board.isGameOver) {
      Dialog.showMessage(contents.head, "Game Over! You hit a mine!", title="Game Over")
    }
  }
}
