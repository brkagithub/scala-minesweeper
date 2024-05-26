//import io.circe.generic.auto._
//import io.circe.syntax._
//import io.circe.parser._

class Cell(var isMine: Boolean) {
  private var revealed: Boolean = false
  private var mark: Boolean = false
  private var adjacentMines: Int = 0

  def reveal(): Unit = {
    revealed = true
  }

  def toggleMark(): Unit = {
    mark = !mark
  }

  def toggleIsMine(): Unit = {
    isMine = !isMine
  }


  def getRevealed: Boolean = revealed
  def getMark: Boolean = mark

  def getAdjacentMines: Int = adjacentMines
  def setAdjacentMines(value: Int) = {
    adjacentMines = value
  }
}
