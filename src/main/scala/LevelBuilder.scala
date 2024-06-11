import scala.swing._
import scala.swing.event._
import java.io.File

class LevelBuilder(var board: Board, difficulty: String, filepath: String, ui: UI) extends BoxPanel(Orientation.Vertical) {
  private val addFirstRowButton = new Button("Add first row")
  private val addLastRowButton = new Button("Add last row")
  private val addFirstColumnButton = new Button("Add first column")
  private val addLastColumnButton = new Button("Add last column")
  private val saveButton = new Button("Save level")
  private val removeFirstRowButton = new Button("Remove first row")
  private val removeLastRowButton = new Button("Remove last row")
  private val removeFirstColumnButton = new Button("Remove first column")
  private val removeLastColumnButton = new Button("Remove last column")
  private val rotationButton = new Button("Rotation")
  private val symmetryButton = new Button("Symmetry")

  private val (minRows, maxRows, minCols, maxCols) = difficulty match {
    case "Beginner" => (1, 10, 1, 10)
    case "Intermediate" => (10, 20, 10, 20)
    case "Expert" => (20, 30, 20, 30)
    case _ => (1, 30, 1, 30)
  }

  contents += new FlowPanel {
    contents += addFirstRowButton
    contents += addLastRowButton
    contents += addFirstColumnButton
    contents += addLastColumnButton
    contents += removeFirstRowButton
    contents += removeLastRowButton
    contents += removeFirstColumnButton
    contents += removeLastColumnButton
    contents += saveButton
    contents += rotationButton
    contents += symmetryButton
  }

  listenTo(addFirstRowButton, addLastRowButton, addFirstColumnButton, addLastColumnButton,
    removeFirstRowButton, removeLastRowButton, removeFirstColumnButton, removeLastColumnButton, saveButton,
    rotationButton, symmetryButton)


  reactions += {
    case ButtonClicked(`addFirstRowButton`) => addFirstRow()
    case ButtonClicked(`addLastRowButton`) => addLastRow()
    case ButtonClicked(`addFirstColumnButton`) => addFirstColumn()
    case ButtonClicked(`addLastColumnButton`) => addLastColumn()
    case ButtonClicked(`removeFirstRowButton`) => removeFirstRow()
    case ButtonClicked(`removeLastRowButton`) => removeLastRow()
    case ButtonClicked(`removeFirstColumnButton`) => removeFirstColumn()
    case ButtonClicked(`removeLastColumnButton`) => removeLastColumn()
    case ButtonClicked(`saveButton`) => saveLevel()
    case ButtonClicked(`rotationButton`) => rotation()
    case ButtonClicked(`symmetryButton`) => symmetry()
  }

  private def addFirstRow(): Unit = {
    if (board.height >= maxRows) {
      Dialog.showMessage(contents.head, s"Cannot add more rows. Maximum rows for $difficulty is $maxRows.", title = "Sorry!")
      return
    }
    board.height += 1
    val newGrid = Array.ofDim[Cell](board.height, board.width)
    Array.copy(board.getGrid, 0, newGrid, 1, board.height - 1)
    newGrid(0) = Array.fill(board.width)(new Cell(isMine = false))
    board.setGrid(newGrid)

    ui.setBoard(board)
    ui.recreateButtons()
    ui.updateUI()
  }

  private def addLastRow(): Unit = {
    if (board.height >= maxRows) {
      Dialog.showMessage(contents.head, s"Cannot add more rows. Maximum rows for $difficulty is $maxRows.", title = "Sorry!")
      return
    }
    board.height += 1
    val newGrid = Array.ofDim[Cell](board.height, board.width)
    Array.copy(board.getGrid, 0, newGrid, 0, board.height - 1)
    newGrid(board.height - 1) = Array.fill(board.width)(new Cell(isMine = false))
    board.setGrid(newGrid)

    ui.setBoard(board)
    ui.recreateButtons()
    ui.updateUI()
  }

  private def addFirstColumn(): Unit = {
    if (board.width >= maxCols) {
      Dialog.showMessage(contents.head, s"Cannot add more columns. Maximum columns for $difficulty is $maxCols.", title = "Sorry!")
      return
    }
    board.width += 1
    val newGrid = Array.ofDim[Cell](board.height, board.width)
    for (i <- 0 until board.height) {
      Array.copy(board.getGrid(i), 0, newGrid(i), 1, board.width - 1)
      newGrid(i)(0) = new Cell(isMine = false)
    }
    board.setGrid(newGrid)

    ui.setBoard(board)
    ui.recreateButtons()
    ui.updateUI()
  }

  private def addLastColumn(): Unit = {
    if (board.width >= maxCols) {
      Dialog.showMessage(contents.head, s"Cannot add more columns. Maximum columns for $difficulty is $maxCols.", title = "Sorry!")
      return
    }
    board.width += 1
    val newGrid = Array.ofDim[Cell](board.height, board.width)
    for (i <- 0 until board.height) {
      Array.copy(board.getGrid(i), 0, newGrid(i), 0, board.width - 1)
      newGrid(i)(board.width - 1) = new Cell(isMine = false)
    }

    board.setGrid(newGrid)

    ui.setBoard(board)
    ui.recreateButtons()
    ui.updateUI()
  }

  private def removeFirstRow(): Unit = {
    if (board.height <= minRows) {
      Dialog.showMessage(contents.head, s"Cannot remove more rows. Minimum rows for $difficulty is $minRows.", title = "Sorry!")
      return
    }
    require(board.height > 1)
    board.height -= 1
    val newGrid = Array.ofDim[Cell](board.height, board.width)
    Array.copy(board.getGrid, 1, newGrid, 0, board.height)
    board.setGrid(newGrid)

    ui.setBoard(board)
    ui.recreateButtons()
    ui.updateUI()
  }

  private def removeLastRow(): Unit = {
    if (board.height <= minRows) {
      Dialog.showMessage(contents.head, s"Cannot remove more rows. Minimum rows for $difficulty is $minRows.", title = "Sorry!")
      return
    }
    require (board.height > 1)
    board.height -= 1
    val newGrid = Array.ofDim[Cell](board.height, board.width)
    Array.copy(board.getGrid, 0, newGrid, 0, board.height)
    board.setGrid(newGrid)

    ui.setBoard(board)
    ui.recreateButtons()
    ui.updateUI()
  }

  private def removeFirstColumn(): Unit = {
    if (board.width <= minCols) {
      Dialog.showMessage(contents.head, s"Cannot remove more columns. Minimum columns for $difficulty is $minCols.", title = "Sorry!")
      return
    }
    require (board.width > 1, "Can't remove")
    board.width -= 1
    val newGrid = Array.ofDim[Cell](board.height, board.width)
    for (i <- 0 until board.height) {
      Array.copy(board.getGrid(i), 1, newGrid(i), 0, board.width)
    }
    board.setGrid(newGrid)

    ui.setBoard(board)
    ui.recreateButtons()
    ui.updateUI()
  }

  private def removeLastColumn(): Unit = {
    if (board.width <= minCols) {
      Dialog.showMessage(contents.head, s"Cannot remove more columns. Minimum columns for $difficulty is $minCols.", title = "Sorry!")
      return
    }
    require (board.width > 1, "Can't remove")
    board.width -= 1
    val newGrid = Array.ofDim[Cell](board.height, board.width)
    for (i <- 0 until board.height) {
      Array.copy(board.getGrid(i), 0, newGrid(i), 0, board.width)
    }
    board.setGrid(newGrid)

    ui.setBoard(board)
    ui.recreateButtons()
    ui.updateUI()
  }

  private def saveLevel(): Unit = {
    print(filepath)
    board.saveLevel(filepath)
  }

  private def rotation(): Unit = {
    val rotation = new NonTransparent with NonExtendable with Rotation {
      override def clockwise: Boolean = false
    }

    board = rotation.apply(board, 2, 3, new Area(board, 1, 0, 2, 4))

    ui.setBoard(board)
    ui.recreateButtons()
    ui.updateUI()
  }

  private def symmetry(): Unit = {
    val symmetry = new Transparent with Extendable with Symmetry {
      override def direction: Char = 'd'
    }

    board = symmetry.apply(board, 0, 3, new Area(board, 2, 3, 3, 4))

    ui.setBoard(board)
    ui.recreateButtons()
    ui.updateUI()
  }
}