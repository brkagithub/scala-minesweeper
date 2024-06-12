import scala.collection.mutable.ArrayBuffer

class Area(board: Board, topLeftX: Int, topLeftY: Int, bottomRightX: Int, bottomRightY: Int) {
  var fields: ArrayBuffer[(Int, Int, Boolean)] = ArrayBuffer()
  var addedFirstRows: Int = 0
  var addedLastRows: Int = 0
  var addedFirstColumns: Int = 0
  var addedLastColumns: Int = 0


  for (x <- topLeftX to bottomRightX; y <- topLeftY to bottomRightY) {
    val isMine = board.getGrid(x)(y).isMine
    fields += ((x, y, isMine))
  }
  def setFields(newFields: ArrayBuffer[(Int, Int, Boolean)]): Unit = {
    fields = newFields
  }

  def addFirstRow(): Unit = {
    addedFirstRows += 1
  }

  def addLastRow(): Unit = {
    addedLastRows += 1
  }

  def addFirstColumn(): Unit = {
    addedFirstColumns += 1
  }

  def addLastColumn(): Unit = {
    addedLastColumns += 1
  }

  def updateBoard(): Board = {
    for (_ <- 0 until addedFirstRows) {
      board.addFirstRow()
    }
    addedFirstRows = 0

    for (_ <- 0 until addedLastRows) {
      board.addLastRow()
    }
    addedLastRows = 0

    for (_ <- 0 until addedFirstColumns) {
      board.addFirstColumn()
    }
    addedFirstColumns = 0

    for (_ <- 0 until addedLastColumns) {
      board.addLastColumn()
    }
    addedLastColumns = 0

    fields.toArray.foreach { case (x, y, isMine) => {
      board.getGrid(x)(y).isMine = isMine
    }
    }

    board.calculateAdjacentMines()

    return board
  }

  def updateCoordinates(area: Area): Unit = {
    for (_ <- 0 until area.addedFirstRows) {
      fields = fields.map { case (x, y, isMine) => (x + 1, y, isMine) }
    }

    for (_ <- 0 until area.addedFirstColumns) {
      fields = fields.map { case (x, y, isMine) => (x, y + 1, isMine) }
    }
  }
}
