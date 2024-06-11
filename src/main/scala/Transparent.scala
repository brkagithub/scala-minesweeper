trait Transparent extends Isometry {
  override def transparent: Boolean = true

  override def apply(board: Board, centerX: Int, centerY: Int, area: Area): Board = {
    area.fields.toArray.foreach {
      case (x, y, isMine) => {
        board.getGrid(x)(y).setIsMine(isMine || board.getGrid(x)(y).isMine)
        board.calculateAdjacentMines()
      }
    }

    board
  }
}
