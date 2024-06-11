//import scala.collection.mutable.ArrayBuffer
//
//trait RotationWorking extends Isometry {
//  def clockwise: Boolean
//
//  //        val subGridWidth = bottomRightX - topLeftX + 1
//  //        val subGridHeight = bottomRightY - topLeftY + 1
//  //    val newGridWidth = if(extendable) math.max(subGridWidth, subGridHeight) + board.getWidth - subGridWidth else board.getWidth
//  //    val newGridHeight = if (extendable) math.max(subGridWidth, subGridHeight) + board.getHeight - subGridHeight else board.getHeight
//
//def apply(board: Board, centerX: Int, centerY: Int, area: Area): Board = {
//    val nextArea = ArrayBuffer[(Int, Int, Boolean)]() // to save previous state of fields in the area
//
//    var minX = 0
//    var maxX = board.height - 1
//    var minY = 0
//    var maxY = board.width - 1
//
//    area.fields.toArray.foreach { case (x, y, isMine) =>
//      val isMine = board.getGrid(x)(y).isMine
//
//      board.getGrid(x)(y).setIsMine(false) // clear previous area from mines
//      val (newX, newY) = rotatePoint(x, y, centerX, centerY, clockwise) // get rotated coordinates
////      println(x, y, isMine)
////      println(newX, newY, isMine)
//
//      super.apply(board, newX, newY, area)
//      // u board ce biti dodati redove
//
//      nextArea += ((newX, newY, isMine))
//      // set new coordinates
//      if (newX < minX) minX = newX
//      if (newX > maxX) maxX = newX
//      if (newY < minY) minY = newY
//      if (newY > maxY) maxY = newY
//    }
//
//    super.apply(board, -1, -1, nextArea)
//    // println(s"Updated minX: $minX, maxX: $maxX, minY: $minY, maxY: $maxY")
//
//    // add new row if x is < 0
//    while(minX < 0){
//      board.addFirstRow()
//      minX += 1
//      maxX += 1
//
//      for (i <- nextArea.indices) {
//        val (x, y, isMine) = nextArea(i)
//        nextArea(i) = (x + 1, y, isMine)
//      }
//    }
//
//    while(minY < 0){
//      board.addFirstColumn()
//      minY += 1
//      maxY += 1
//
//      for (i <- nextArea.indices) {
//        val (x, y, isMine) = nextArea(i)
//        nextArea(i) = (x, y + 1, isMine)
//      }
//    }
//
//    while(maxX > board.height - 1){
//      board.addLastRow()
//    }
//
//    while (maxY > board.width - 1) {
//      board.addLastColumn()
//    }
//
//    nextArea.toArray.foreach {
//      case (x, y, isMine) => {
//        board.getGrid(x)(y).setIsMine(isMine)
//      }
//    }
//
//    board
//  }
//
//  private def rotatePoint(x: Int, y: Int, centerX: Int, centerY: Int, clockwise: Boolean = true): (Int, Int) = {
//    val dx = x - centerX
//    val dy = y - centerY
//    if (clockwise) {
//      (centerX + dy, centerY - dx)
//    } else {
//      (centerX - dy, centerY + dx)
//    }
//  }
//}