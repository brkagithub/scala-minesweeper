import scala.collection.mutable.ArrayBuffer
trait Symmetry extends Isometry {
  def direction: Char
  // h = horizontal, v = vertical, d = diagonal (main)

  override def apply(board: Board, centerX: Int, centerY: Int, area: Area): Area = {
    val newAreaFields = ArrayBuffer[(Int, Int, Boolean)]()

    area.fields.toArray.foreach { case (x, y, isMine) =>
      val isMine = board.getGrid(x)(y).isMine

//      board.getGrid(x)(y).setIsMine(false) // clear previous area from mines
      val (newX, newY) = transformPoint(x, y, centerX, centerY, direction) // get rotated coordinates

      newAreaFields += ((newX, newY, isMine))
    }

    val newArea = new Area(board, 0, 0, 0, 0)
    newArea.setFields(newAreaFields)

    super.apply(board, -1, -1, newArea)
  }

  private def transformPoint(x: Int, y: Int, centerX: Int, centerY: Int, direction: Char): (Int, Int) = {
    direction match {
      case 'h' => (2 * centerX - x, y)
      case 'v' => (x, 2 * centerY - y)
      case 'd' => (centerX + centerY - y, centerX + centerY - x)
      case _ => (x, y)
    }
  }
}

