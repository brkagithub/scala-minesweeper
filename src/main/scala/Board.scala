import scala.annotation.tailrec
import scala.util.Random

class Board(val width: Int, val height: Int, val numMines: Int) {
  private var gameOver: Boolean = false
  private var gameWon: Boolean = false
  private val grid = Array.tabulate(height, width)((_, _) => new Cell(isMine = false))

  def isGameOver: Boolean = gameOver
  def isGameWon: Boolean = gameWon

  def getGrid: Array[Array[Cell]] = grid;

  init()

  private def printBoardDebug(): Unit = {
    for (i <- 0 until height) {
      for (j <- 0 until width) {
        print(if (grid(i)(j).isMine) " * " else " " + grid(i)(j).getAdjacentMines.toString + " ")
      }
      println()
    }
    println()
  }

  private def init(): Unit = {
    placeMines()
    calculateAdjacentMines()
//    printBoardDebug()
  }

  private def placeMines(): Unit = {
    @tailrec
    def placeMinesRec(minesPlaced: Int): Unit = {
      if(minesPlaced < numMines){
        val x = Random.nextInt(height)
        val y = Random.nextInt(width)

        if(!grid(x)(y).isMine){
          grid(x)(y) = new Cell(true)
          placeMinesRec(minesPlaced + 1)
        }
        else{
          placeMinesRec(minesPlaced)
        }
      }
    }

    placeMinesRec(0)
  }

  private def calculateAdjacentMines(): Unit = {
    for {
      i <- 0 until height
      j <- 0 until width
      if !grid(i)(j).isMine
    } {
      val adjacentMines = countAdjacentMines(i, j)
      grid(i)(j).setAdjacentMines(adjacentMines)
    }
  }

  private def countAdjacentMines(row: Int, col: Int): Int = {
    val neighbourIndices =
      for {
        dx <- -1 to 1
        dy <- -1 to 1
        if dx != 0 || dy != 0
        x = row + dx
        y = col + dy
        if x >= 0 && x < height && y >= 0 && y < width
        if grid(x)(y).isMine
      } yield (x, y)

    neighbourIndices.length
  }

  def revealCell(row: Int, col: Int): Unit = {
    require(!gameOver, "Game is already over")
    require(row >= 0 && row < height && col >= 0 && col < width, "Invalid cell coordinates")

    val cell = grid(row)(col)
    if(!cell.getRevealed && !cell.getMark){
      if(cell.isMine){
        cell.reveal()
        gameOver = true
        revealAllMines()
      }
      else {
        checkWinCondition()
        if (cell.getAdjacentMines == 0) {
          revealAdjacentCells(row, col)
        }
        cell.reveal()
      }
    }
  }

  private def revealAllMines(): Unit = {
    for (row <- grid; cell <- row if cell.isMine) {
      cell.reveal()
    }
  }

  private def revealAdjacentCells(row: Int, col: Int): Unit = {
    @tailrec
    def revealCellRecursive(toReveal: List[(Int, Int)]): Unit = toReveal match {
      case Nil => // Base case
      case (r, c) :: rest => // (r, c) is head, rest is tail
        val cell = grid(r)(c)
        if (!cell.getRevealed && !cell.getMark) {
          cell.reveal()
          if (cell.getAdjacentMines == 0) {
            val neighbors = for {
              dx <- -1 to 1
              dy <- -1 to 1
              if !(dx == 0 && dy == 0)
              nr = r + dx
              nc = c + dy
              if nr >= 0 && nr < height && nc >= 0 && nc < width
              if !grid(nr)(nc).getRevealed && !grid(nr)(nc).getMark
            } yield (nr, nc)

            revealCellRecursive(rest ++ neighbors) // combines two Lists
          } else {
            revealCellRecursive(rest)
          }
        } else {
          revealCellRecursive(rest)
        }
    }

    revealCellRecursive(List((row, col)))
  }

  private def checkWinCondition(): Unit = {
    val unrevealedNonMineCells = grid.flatten.count(cell => !cell.isMine && !cell.getRevealed)
    if(unrevealedNonMineCells == 0) {
      gameWon = true
      gameOver = true
    }
  }
}
