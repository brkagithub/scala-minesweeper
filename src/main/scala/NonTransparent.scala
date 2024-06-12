trait NonTransparent extends Isometry {
  override def transparent: Boolean = false

  override def apply(board: Board, centerX: Int, centerY: Int, area: Area): Area = {
    area
  }
}
