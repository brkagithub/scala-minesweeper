import scala.collection.mutable.ArrayBuffer
trait Rotation extends Isometry {
  def clockwise: Boolean

  //        val subGridWidth = bottomRightX - topLeftX + 1
  //        val subGridHeight = bottomRightY - topLeftY + 1
  //    val newGridWidth = if(extendable) math.max(subGridWidth, subGridHeight) + board.getWidth - subGridWidth else board.getWidth
  //    val newGridHeight = if (extendable) math.max(subGridWidth, subGridHeight) + board.getHeight - subGridHeight else board.getHeight

override def apply(board: Board, centerX: Int, centerY: Int, area: Area): Board = {
    val newAreaFields = ArrayBuffer[(Int, Int, Boolean)]() // to save previous state of fields in the area

    area.fields.toArray.foreach { case (x, y, isMine) =>
      val isMine = board.getGrid(x)(y).isMine

      board.getGrid(x)(y).setIsMine(false) // clear previous area from mines
      val (newX, newY) = rotatePoint(x, y, centerX, centerY, clockwise) // get rotated coordinates

      newAreaFields += ((newX, newY, isMine))
    }

    val newArea = new Area(board, 0, 0, 0, 0)
    newArea.setFields(newAreaFields)
    val newBoard = super.apply(board, -1, -1, newArea)
    newBoard
  }

  private def rotatePoint(x: Int, y: Int, centerX: Int, centerY: Int, clockwise: Boolean = true): (Int, Int) = {
    val dx = x - centerX
    val dy = y - centerY
    if (clockwise) {
      (centerX + dy, centerY - dx)
    } else {
      (centerX - dy, centerY + dx)
    }
  }
}