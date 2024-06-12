trait Transparent extends Isometry {
  override def transparent: Boolean = true

  override def apply(board: Board, centerX: Int, centerY: Int, area: Area): Area = {
    val newFields = area.fields.map {
      case (x, y, isMine) =>
        val boardStateXY = if (x >= 0 && x < board.height && y >= 0 && y < board.width) board.getGrid(x)(y).isMine else false
        val newIsMine = isMine || boardStateXY
        (x, y, newIsMine)
    }
    area.setFields(newFields)

    area
  }
}
