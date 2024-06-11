trait Extendable extends Isometry {
  override def extendable: Boolean = true
  override def apply(board: Board, centerX: Int, centerY: Int, area: Area): Board = {
    var minX = 0
    var maxX = board.height - 1
    var minY = 0
    var maxY = board.width - 1

    area.fields.toArray.foreach { case (x, y, isMine) =>
      if (x < minX) minX = x
      if (x > maxX) maxX = x
      if (y < minY) minY = y
      if (y > maxY) maxY = y
    }

    val nextArea = area.fields

    while (minX < 0) {
      board.addFirstRow()
      minX += 1
      maxX += 1

      for (i <- nextArea.indices) {
        val (x, y, isMine) = nextArea(i)
        nextArea(i) = (x + 1, y, isMine)
      }
    }

    while (minY < 0) {
      board.addFirstColumn()
      minY += 1
      maxY += 1

      for (i <- nextArea.indices) {
        val (x, y, isMine) = nextArea(i)
        nextArea(i) = (x, y + 1, isMine)
      }
    }

    while (maxX > board.height - 1) {
      board.addLastRow()
    }

    while (maxY > board.width - 1) {
      board.addLastColumn()
    }

    super.apply(board, -1, -1, area)
  }
}
