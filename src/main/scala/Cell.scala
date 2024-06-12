//import io.circe.generic.auto._
//import io.circe.syntax._
//import io.circe.parser._

class Cell(var isMine: Boolean) {
   var revealed: Boolean = false
   var mark: Boolean = false
   var adjacentMines: Int = 0

  def reveal(): Unit = {
    revealed = true
  }

  def toggleMark(): Unit = {
    mark = !mark
  }

  def toggleIsMine(): Unit = {
    isMine = !isMine
  }

  def setIsMine(value: Boolean): Unit = {
    isMine = value
  }


  def getRevealed: Boolean = revealed
  def getMark: Boolean = mark

  def getAdjacentMines: Int = adjacentMines
  def setAdjacentMines(value: Int) = {
    adjacentMines = value
  }
}
