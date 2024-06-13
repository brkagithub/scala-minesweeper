import scala.swing._
import scala.swing.event._
import java.io.{BufferedWriter, FileWriter, IOException}
import java.io.File
import scala.io.Source
import scala.util.matching.Regex

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

  private val createButton = new Button("Create")
  private val applyButton = new Button("Apply")

  private val operationTypeComboBox = new ComboBox(Seq("Rotation", "Symmetry", "Custom"))
  private val transparencyComboBox = new ComboBox(Seq("Transparent", "NonTransparent"))
  private val extendabilityComboBox = new ComboBox(Seq("Extendable", "NonExtendable"))
  private val rotationDirectionComboBox = new ComboBox(Seq("Clockwise", "CounterClockwise"))
  private val symmetryDirectionComboBox = new ComboBox(Seq("h", "v", "d"))

  private val centerXField = new TextField {
    columns = 5
  }
  private val centerYField = new TextField {
    columns = 5
  }
  private val topLeftXField = new TextField {
    columns = 5
  }
  private val topLeftYField = new TextField {
    columns = 5
  }
  private val bottomRightXField = new TextField {
    columns = 5
  }
  private val bottomRightYField = new TextField {
    columns = 5
  }
  private val addCustomIsometryField = new TextField {
    columns = 80
  }
  private val customField = new TextField {
    columns = 80
  }


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
  }

  contents += new FlowPanel {
    contents += new Label("Operation Type:")
    contents += operationTypeComboBox
    contents += new Label("Transparency:")
    contents += transparencyComboBox
    contents += new Label("Extendability:")
    contents += extendabilityComboBox
    contents += new Label("Rotation Direction:")
    contents += rotationDirectionComboBox
    contents += new Label("Symmetry Direction:")
    contents += symmetryDirectionComboBox
  }

  contents += new FlowPanel {
    contents += new Label("Create:")
    contents += addCustomIsometryField
    contents += createButton
  }

  contents += new FlowPanel {
    contents += new Label("Execute:")
    contents += customField
  }

  contents += new FlowPanel {
    contents += new Label("Center X:")
    contents += centerXField
    contents += new Label("Center Y:")
    contents += centerYField
    contents += new Label("Top Left X:")
    contents += topLeftXField
    contents += new Label("Top Left Y:")
    contents += topLeftYField
    contents += new Label("Bottom Right X:")
    contents += bottomRightXField
    contents += new Label("Bottom Right Y:")
    contents += bottomRightYField
    contents += applyButton
  }

  listenTo(addFirstRowButton, addLastRowButton, addFirstColumnButton, addLastColumnButton,
    removeFirstRowButton, removeLastRowButton, removeFirstColumnButton, removeLastColumnButton, saveButton,
    applyButton, createButton, operationTypeComboBox.selection, transparencyComboBox.selection, extendabilityComboBox.selection,
    rotationDirectionComboBox.selection, symmetryDirectionComboBox.selection)


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
    case ButtonClicked(`applyButton`) => applyOperation()
    case ButtonClicked(`createButton`) => createIsometry()
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
    require(board.height > 1)
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
    require(board.width > 1, "Can't remove")
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
    require(board.width > 1, "Can't remove")
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

  private def appendLineToFile(filePath: String, inputText: String): Unit = {
    var bw: BufferedWriter = null
    try {
      bw = new BufferedWriter(new FileWriter(filePath, true))
      bw.write(inputText)
      bw.newLine()
    } catch {
      case e: IOException => e.printStackTrace()
    } finally {
      try {
        if (bw != null) bw.close()
      } catch {
        case e: IOException => e.printStackTrace()
      }
    }
  }
  private def createIsometry(): Unit = {
    val filePath = "./temp/isometries.txt"
    val inputText = addCustomIsometryField.text
    appendLineToFile(filePath, inputText)
  }
  private def applyOperation(): Unit = {
    val isTransparent = transparencyComboBox.selection.item == "Transparent"
    val isExtendable = extendabilityComboBox.selection.item == "Extendable"
    val operationType = operationTypeComboBox.selection.item
    val rotationClockwise = rotationDirectionComboBox.selection.item == "Clockwise"
    val symmetryDirection = symmetryDirectionComboBox.selection.item.head

    val centerX = centerXField.text.toInt
    val centerY = centerYField.text.toInt
    val topLeftX = topLeftXField.text.toInt
    val topLeftY = topLeftYField.text.toInt
    val bottomRightX = bottomRightXField.text.toInt
    val bottomRightY = bottomRightYField.text.toInt

    // dummy obj
    var oldArea: Area = new Area(board, 0, 0, 0, 0)
    var area: Area = new Area(board, 0, 0, 0, 0)

    if (operationType == "Rotation") {
      if (isTransparent) {
        if (isExtendable) {
          val rotation = new Transparent with Extendable with Rotation {
            override def clockwise: Boolean = rotationClockwise
          }
          oldArea = new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY)
          area = rotation.apply(board, centerX, centerY, new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY))
        } else {
          val rotation = new Transparent with NonExtendable with Rotation {
            override def clockwise: Boolean = rotationClockwise
          }
          oldArea = new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY)
          area = rotation.apply(board, centerX, centerY, new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY))
        }
      } else {
        if (isExtendable) {
          val rotation = new NonTransparent with Extendable with Rotation {
            override def clockwise: Boolean = rotationClockwise
          }
          oldArea = new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY)
          area = rotation.apply(board, centerX, centerY, new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY))
        } else {
          val rotation = new NonTransparent with NonExtendable with Rotation {
            override def clockwise: Boolean = rotationClockwise
          }
          oldArea = new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY)
          area = rotation.apply(board, centerX, centerY, new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY))
        }
      }
    } else if (operationType == "Symmetry") {
      if (isTransparent) {
        if (isExtendable) {
          val symmetry = new Transparent with Extendable with Symmetry {
            override def direction: Char = symmetryDirection
          }
          oldArea = new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY)
          area = symmetry.apply(board, centerX, centerY, new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY))
        } else {
          val symmetry = new Transparent with NonExtendable with Symmetry {
            override def direction: Char = symmetryDirection
          }
          oldArea = new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY)
          area = symmetry.apply(board, centerX, centerY, new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY))
        }
      } else {
        if (isExtendable) {
          val symmetry = new NonTransparent with Extendable with Symmetry {
            override def direction: Char = symmetryDirection
          }
          oldArea = new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY)
          area = symmetry.apply(board, centerX, centerY, new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY))
        } else {
          val symmetry = new NonTransparent with NonExtendable with Symmetry {
            override def direction: Char = symmetryDirection
          }
          oldArea = new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY)
          area = symmetry.apply(board, centerX, centerY, new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY))
        }
      }
    } else {
      // custom
      // S = symetry, R = rotation
      // C = clockwise CC = counterclockwise
      // D = diagonal, H = horizontal, V = vertical
      // T = transparent, NT = nontransparent
      // E = extendable, NE = nonextendable

      // R_CC_NE_NT(2,3) -> S_V_E_NT(3,1)
      val customCall = customField.text
      // centerX, centerY

      val isometries = readIsometries("./temp/isometries.txt")
      val customInput = getIsometryString(customCall, centerX, centerY, isometries)
      println(customInput)
//      val customInput = customField.text
      val c = new Composition()
      val (isometryArray, parameterArray) = c.parseCustomInput(customInput)

      val copiedBoard = board.copyBoard()
      area = new Area(copiedBoard, topLeftX, topLeftY, bottomRightX, bottomRightY)

      val composedIsometry = c.compose(copiedBoard, isometryArray)
      composedIsometry(area, parameterArray)

      board = copiedBoard

//      c.executeIsometries(copiedBoard, area, isometryArray, parameterArray)
//      val rotation = new NonTransparent with NonExtendable with Rotation {
//        override def clockwise: Boolean = false
//      }
//      println((topLeftX, topLeftY, bottomRightX, bottomRightY))
//      oldArea = new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY)
//
//      area = rotation.apply(board, 2, 3, new Area(board, topLeftX, topLeftY, bottomRightX, bottomRightY))
//
//      oldArea.updateCoordinates(area)
//
//      board = area.updateBoard()
//
//      println("cleaning original fields which are not in image")
//      // clean original which are not in image
//      val areaCoordinates = area.fields.map { case (x, y, _) => (x, y) }.toSet
//
//      oldArea.fields.toArray.foreach { case (x, y, isMine) =>
//        if (!areaCoordinates.contains((x, y))) {
//          board.getGrid(x)(y).setIsMine(false)
//        }
//      }
//
//      val symmetry = new NonTransparent with Extendable with Symmetry {
//        override def direction: Char = 'v'
//      }
//
//      oldArea = area.copyArea()
//      val areaCopy = area.copyArea()
//
//      area = symmetry.apply(board, 3, 1, areaCopy)
    }
    if(operationType != "Custom"){
      // update board once
      oldArea.updateCoordinates(area)

      board = area.updateBoard()

      println("cleaning original fields which are not in image")
      // clean original which are not in image
      val areaCoordinates = area.fields.map { case (x, y, _) => (x, y) }.toSet

      oldArea.fields.toArray.foreach { case (x, y, isMine) =>
        if (!areaCoordinates.contains((x, y))) {
          board.getGrid(x)(y).setIsMine(false)
        }
      }
    }
    board.calculateAdjacentMines()

    ui.setBoard(board)
    ui.recreateButtons()
    ui.updateUI()
  }

  private def readIsometries(filePath: String): Map[String, String] = {
    val source = Source.fromFile(filePath)
    val isometries = source.getLines().collect {
      case line if line.contains("=") =>
        val parts = line.split("=").map(_.trim)
        parts(0).split("\\(")(0) -> parts(1)
    }.toMap
    source.close()
    isometries
  }

  private def getIsometryString(input: String, centerX: Int, centerY: Int, isometries: Map[String, String]): String = {
    val pattern: Regex = """([A-Za-z0-9_]+)\(([^)]+)\)""".r

    input match {
      case pattern(name, _) if isometries.contains(name) =>
        val isometryTemplate = isometries(name)
        val result = isometryTemplate.replaceAllLiterally("x", centerX.toString).replaceAllLiterally("y", centerY.toString)
        result
      case _ => "Isometry not found"
    }
  }
}