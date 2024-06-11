trait Isometry {
  def extendable: Boolean
  def transparent: Boolean
  def apply(board: Board, centerX: Int, centerY: Int, area: Area): Board = {
    print("done")

    board
  }
}
