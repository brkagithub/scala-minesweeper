import scala.swing._
import scala.swing.event._

class UI(width: Int, height: Int, numMines: Int) extends GridPanel(height, width) {
  val board = new Board(width, height, numMines)
  val buttons = Array.tabulate(height, width) { (row, col) =>
    new Button {
      preferredSize = new Dimension(40, 40)
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

  contents ++= buttons.flatten.toSeq

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
  }

  def checkGameOver(): Unit = {
    if (board.isGameWon) {
      Dialog.showMessage(contents.head, "Congratulations! You won!", title="Victory")
    } else if (board.isGameOver) {
      Dialog.showMessage(contents.head, "Game Over! You hit a mine!", title="Game Over")
    }
  }
}
