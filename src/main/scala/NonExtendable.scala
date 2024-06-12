trait NonExtendable extends Isometry {
  override def extendable: Boolean = false

  override def apply(board: Board, centerX: Int, centerY: Int, area: Area): Area = {
    val minX = 0
    val maxX = board.height - 1
    val minY = 0
    val maxY = board.width - 1

    area.fields = area.fields.filter { case (x, y, _) =>
      x >= minX && x <= maxX && y >= minY && y <= maxY
    }

    super.apply(board, -1, -1, area)
  }
}
