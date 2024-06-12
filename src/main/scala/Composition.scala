import scala.util.matching.Regex

class Composition {
  def parseCustomInput(input: String) = {
    val parts = input.split(" -> ").map(_.trim)

    val isometryPattern: Regex = """([A-Z]+(?:_[A-Z]+)*)(\(\d+,\d+\))""".r

    def parseIsometry(isometry: String) = {
      isometry match {
        case isometryPattern(types, args) =>
          // Split the types by "_"
          val typesArray = types.split("_")
          // Extract the arguments (x, y) from the matched pattern
          val argsPattern: Regex = """\((\d+),(\d+)\)""".r
          val argsPattern(x, y) = args

          // remove ()
          typesArray.dropRight(1)

          (typesArray, (x.toInt, y.toInt))
      }
    }

    val parsedIsometries = parts.map(parseIsometry)
    var isometryArray = Array[Isometry]()
    var parameterArray = Array[(Int, Int)]()

    parsedIsometries.foreach { isometry =>
      val (types, coords) = isometry

      if(types.length > 1){
        // is basic isometry

        // dummy obj, to be changed
        var isometry: Isometry = new Isometry {
          override def extendable: Boolean = true

          override def transparent: Boolean = true
        }

        if(types(0) == "R")
        {
          // rotation
          val isClockwise: Boolean = types(1) == "C"

          if(types(2) == "E" && types(3) == "T"){
            isometry = new Transparent with Extendable with Rotation {
              override def clockwise: Boolean = isClockwise
            }
          }
          else if(types(2) == "NE" && types(3) == "T"){
            isometry = new NonTransparent with Extendable with Rotation {
              override def clockwise: Boolean = isClockwise
            }
          }
          else if (types(2) == "E" && types(3) == "NT") {
            isometry = new Transparent with NonExtendable with Rotation {
              override def clockwise: Boolean = isClockwise
            }
          }
          else if (types(2) == "NE" && types(3) == "NT") {
            isometry = new NonTransparent with NonExtendable with Rotation {
              override def clockwise: Boolean = isClockwise
            }
          }
        }
        else {
          val directionArr = types(1).toString().toLowerCase().toCharArray()
          val directionChosen = directionArr(0)

          if (types(2) == "E" && types(3) == "T") {
            isometry = new Transparent with Extendable with Symmetry {
              override def direction: Char = directionChosen
            }
          }
          else if (types(2) == "NE" && types(3) == "T") {
            isometry = new Transparent with NonExtendable with Symmetry {
              override def direction: Char = directionChosen
            }
          }
          else if (types(2) == "E" && types(3) == "NT") {
            isometry = new NonTransparent with Extendable with Symmetry {
              override def direction: Char = directionChosen
            }
          }
          else if (types(2) == "NE" && types(3) == "NT") {
            isometry = new NonTransparent with NonExtendable with Symmetry {
              override def direction: Char = directionChosen
            }
          }
        }

        isometryArray :+= isometry
        parameterArray :+= coords
      }
      else {
        // is named isometry
      }
    }

    (isometryArray, parameterArray)
  }

  def executeIsometries(boardPassed: Board, areaPassed: Area, isometries: Array[Isometry], params: Array[(Int, Int)]): Area = {
    var area = areaPassed
    var board = boardPassed
    if(isometries.length > 0) {
      val isometry = isometries(0)
      println(isometries.length)
      val coordinates = params(0)
      val (x, y) = coordinates

      val oldArea = area.copyArea()

      area = isometry.apply(board, x, y, area.copyArea())

      oldArea.updateCoordinates(area)

      board = area.updateBoard()

      println("cleaning original fields which are not in image")

      val areaCoordinates = area.fields.map { case (x, y, _) => (x, y) }.toSet

      oldArea.fields.toArray.foreach { case (x, y, isMine) =>
        if (!areaCoordinates.contains((x, y))) {
          board.getGrid(x)(y).setIsMine(false)
        }
      }

      board.printBoardDebug()

      executeIsometries(board, area, isometries.drop(1), params.drop(1))
    }
    else {
      area
    }
  }
  def compose(board: Board, isometries: Array[Isometry]) = {
    (area: Area, params: Array[(Int, Int)]) => {
      executeIsometries(board, area, isometries, params)
    }
  }
}
