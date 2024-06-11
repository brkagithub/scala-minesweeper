import scala.collection.mutable.ArrayBuffer

class Area(board: Board, topLeftX: Int, topLeftY: Int, bottomRightX: Int, bottomRightY: Int) {
  var fields: ArrayBuffer[(Int, Int, Boolean)] = ArrayBuffer()

  for (x <- topLeftX to bottomRightX; y <- topLeftY to bottomRightY) {
    val isMine = board.getGrid(x)(y).isMine
    fields += ((x, y, isMine))
  }
  def setFields(newFields: ArrayBuffer[(Int, Int, Boolean)]): Unit = {
    fields = newFields
  }
}
