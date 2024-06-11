import scala.annotation.tailrec
import scala.util.Random
import java.util.Timer
import java.util.TimerTask
import scala.io.Source
import java.io.{File, PrintWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

class Board(var width: Int, var height: Int, var numMines: Int, difficulty: String, var isInteractive: Boolean = true) {
  private var gameOver: Boolean = false
  private var gameWon: Boolean = false
  private var grid = Array.tabulate(height, width)((_, _) => new Cell(isMine = false))
  private var startTime: Long = 0
  private var elapsedTime: Long = 0
  private var clickCount: Int = 0
  private val timer = new Timer()
  private val scoreFile = new File("./temp/scores.txt")

  def getWidth: Int = width
  def getHeight: Int = height
  def isGameOver: Boolean = gameOver
  def isGameWon: Boolean = gameWon

  def getGrid: Array[Array[Cell]] = grid;

  def setGrid(newGrid: Array[Array[Cell]]): Unit = {
    grid = newGrid
  }
  def getElapsedTime: Long = elapsedTime
  def getClickCount: Int = clickCount

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
    printBoardDebug()
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

  def calculateActualNumberOfMines(): Unit = {
    var actualNumMines = 0

    for {
      i <- 0 until height
      j <- 0 until width
    } {
      if (grid(i)(j).isMine) {
        actualNumMines += 1
      }
    }

    numMines = actualNumMines
  }
  def calculateAdjacentMines(): Unit = {
    for {
      i <- 0 until height
      j <- 0 until width
      if !grid(i)(j).isMine
    } {
      val adjacentMines = countAdjacentMines(i, j)
      grid(i)(j).setAdjacentMines(adjacentMines)
    }

    calculateActualNumberOfMines()
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
    if (!isInteractive) return

    val cell = grid(row)(col)
    if (startTime == 0) startGameTimer()
    if(!cell.getRevealed && !cell.getMark){
      clickCount += 1
      if(cell.isMine){
        cell.reveal()
        gameOver = true
        revealAllMines()
      }
      else {
        if (cell.getAdjacentMines == 0) {
          revealAdjacentCells(row, col)
        }
        cell.reveal()
        checkWinCondition()
      }
    }
  }

  private def revealAllMines(): Unit = {
    for (row <- grid; cell <- row if cell.isMine) {
      cell.reveal()
    }
  }

  private def revealAllCells(): Unit = {
    for (row <- grid; cell <- row) {
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
      timer.cancel()
      saveScore(calculateScore())
    }
  }

  def startGameTimer(): Unit = {
    startTime = System.currentTimeMillis()
    timer.scheduleAtFixedRate(new TimerTask {
      def run(): Unit = {
        elapsedTime = (System.currentTimeMillis() - startTime) / 1000
      }
    }, 1000, 1000)
  }

  def calculateScore(): Double = {
    if (elapsedTime == 0 && clickCount == 0) 0 else 10000.0 / (elapsedTime / clickCount + clickCount)
  }

  private def saveScore(score: Double): Unit = {
    val entry = ScoreEntry(LocalDateTime.now(), score)
    val scores = loadScores() :+ entry // append to end of List
    val sortedScores = scores.sortBy(- _.score) // sort by descending score
    val writer = new PrintWriter(scoreFile)
    try {
      sortedScores.foreach { scoreEntry =>
        writer.println(scoreEntry)
      }
    } finally {
      writer.close()
    }
  }

  def loadScores(): List[ScoreEntry] = {
    if (!scoreFile.exists()) return Nil
    val source = Source.fromFile(scoreFile)
    try {
      source.getLines().map { line =>
        val Array(dateTimeStr, scoreStr) = line.split(": ")
        val dateTime = LocalDateTime.parse(dateTimeStr, DateTimeFormatter.ofPattern("yyyy-dd-MM HH:mm:ss"))
        val score = scoreStr.toDouble
        ScoreEntry(dateTime, score)
      }.toList
    } finally {
      source.close()
    }
  }

  def recommendMove(): Option[(Int, Int)] = {
    for {
      i <- 0 until height
      j <- 0 until width
      if !grid(i)(j).isMine && !grid(i)(j).getRevealed
    } {
      clickCount += 10
      revealCell(i, j)
      return Some((i, j))
    }
    None
  }

  def saveLevel(filenamePassed: String = ""): Unit = {
    val filename = if (filenamePassed.isEmpty) {
      val now = LocalDateTime.now()
      val formatter = DateTimeFormatter.ofPattern("yyyy-dd-MM-HH-mm-ss")
      s"./temp/${difficulty}/${now.format(formatter)}.txt"
    } else {
      filenamePassed
    }

    val dir = new File(s"./temp/$difficulty")
    if (!dir.exists()) {
      dir.mkdirs()
    }

    val writer = new PrintWriter(filename)
    try {
      writer.println(s"Width: $width")
      writer.println(s"Height: $height")
      writer.println(s"NumMines: $numMines")
      writer.println(s"ElapsedTime: $elapsedTime")
      writer.println(s"ClickCount: $clickCount")

      for (i <- 0 until height) {
        for (j <- 0 until width) {
          val cell = grid(i)(j)
          val cellState = if (cell.isMine) "*" else cell.getAdjacentMines.toString
          val revealed = if (cell.getRevealed) "R" else "U"
          val marked = if (cell.getMark) "M" else "N"
          writer.print(s"$cellState$revealed$marked ")
        }
        writer.println()
      }
    } finally {
      writer.close()
    }
  }

  def loadLevel(filename: String): Unit = {
    val source = Source.fromFile(filename)
    try {
      val lines = source.getLines().toArray

      width = lines(0).split(": ")(1).toInt
      height = lines(1).split(": ")(1).toInt
      val numMines = lines(2).split(": ")(1).toInt
      elapsedTime = lines(3).split(": ")(1).toLong
      clickCount = lines(4).split(": ")(1).toInt

      val newGrid = Array.tabulate(height, width)((_, _) => new Cell(isMine = false))

      for (i <- 0 until height) {
        val cells = lines(i + 5).trim.split(" ")
        for (j <- 0 until width) {
          val cellStr = cells(j)
          val isMine = cellStr(0) == '*'
          val adjacentMines = if (isMine) 0 else cellStr(0).asDigit
          val revealed = cellStr(1) == 'R'
          val marked = cellStr(2) == 'M'

          val cell = new Cell(isMine)
          cell.setAdjacentMines(adjacentMines)
          if (revealed) cell.reveal()
          if (marked) cell.toggleMark()

          newGrid(i)(j) = cell
        }
      }
      setGrid(newGrid)
    }
  }

  def playTurnsFromFile(filename: String): Unit = {
    val source = Source.fromFile(filename)
    try {
      for(line <- source.getLines()){
        val turn = line.trim
        if(turn.nonEmpty){
          val action = turn.charAt(0)
          val coordinates = turn.substring(2, turn.length - 1).split(",")
          if(coordinates.length == 2){
            val x = coordinates(0).toInt
            val y = coordinates(1).toInt
            if(x >= 0 && x < height && y >=0 && y < width){
              action match {
                case 'L' => revealCell(x, y)
                case 'D' => grid(x)(y).toggleMark()
                case _ =>
              }
            }
          }
        }
      }
    }
  }

  def addFirstRow(): Unit = {
    height += 1
    val newGrid = Array.ofDim[Cell](height, width)
    Array.copy(getGrid, 0, newGrid, 1, height - 1)
    newGrid(0) = Array.fill(width)(new Cell(isMine = false))
    setGrid(newGrid)
  }

  def addLastRow(): Unit = {
    height += 1
    val newGrid = Array.ofDim[Cell](height, width)
    Array.copy(getGrid, 0, newGrid, 0, height - 1)
    newGrid(height - 1) = Array.fill(width)(new Cell(isMine = false))
    setGrid(newGrid)
  }

  def addFirstColumn(): Unit = {
    width += 1
    val newGrid = Array.ofDim[Cell](height, width)
    for (y <- 0 until height) {
      Array.copy(getGrid(y), 0, newGrid(y), 1, width - 1)
      newGrid(y)(0) = new Cell(isMine = false)
    }
    setGrid(newGrid)
  }

  def addLastColumn(): Unit = {
    width += 1
    val newGrid = Array.ofDim[Cell](height, width)
    for (y <- 0 until height) {
      Array.copy(getGrid(y), 0, newGrid(y), 0, width - 1)
      newGrid(y)(width - 1) = new Cell(isMine = false)
    }
    setGrid(newGrid)
  }
}
