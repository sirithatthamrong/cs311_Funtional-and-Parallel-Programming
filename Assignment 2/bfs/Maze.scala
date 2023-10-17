object Maze extends App {

  case class Letter(row: Int, col: Int, myChar: Char) {
    def isNearBy(that: Letter): Boolean = {
      math.abs(that.row - this.row) + math.abs(that.col - this.col) == 1
    }
    def direction(that: Letter): Char = (that.row - this.row, that.col - this.col) match {
      case (-1, 0) => 'd'
      case (1, 0)  => 'u'
      case (0, -1) => 'r'
      case (0, 1)  => 'l'
      case _ => ' '
    }
    def isFree: Boolean = myChar.equals(' ')
    def isSrc: Boolean = myChar.equals('s')
    def isExit: Boolean = myChar.equals('e')
  }

  def solveMaze(maze: Vector[String]): Option[String] = {
    val letters: Vector[Letter] = maze
      .zipWithIndex // define row -> [("xxxxxxxx", 0), ("x x x ", 1), .....]
      .flatMap {    // ("xxxxxxxx", 0), ("x x x ", 1), ....
        case (row, rowIndex) =>
          row.zipWithIndex // define column
            .map {         // separate each char, and store row & col individually
            case (char, colIndex) =>
              Letter(rowIndex, colIndex, char)
          }
      }
    val src: Option[Letter] = letters.find(_.isSrc)
    val exit: Option[Letter] = letters.find(_.isExit)
    val nbrsMap: Map[Letter, Set[Letter]] = letters.foldLeft(Map.empty[Letter, Set[Letter]]) { (acc, item) =>
      val nbrs = letters.filter(myNbrs => item.isNearBy(myNbrs) && (myNbrs.isFree || myNbrs.isExit)).toSet
      acc + (item -> nbrs)
    }
    def findPath(src: Letter, dest: Letter, bfsResult: Map[Letter, Letter]): Option[String] = {
      def constructPath(dest: Letter, allPath: String): Option[String] = {
        if (dest.equals(src)) Some(allPath)
        // bfsResult = Parent of dest
        else constructPath(bfsResult(dest), dest.direction(bfsResult(dest)) + allPath)
      }
      if (bfsResult.contains(dest)) constructPath(dest, "")
      else None
    }
    val (parentResult, distance) = GraphBFS.bfs(nbrsMap, src.get)
    findPath(src.get, exit.get, parentResult)
  }
//  val maze: Vector[String] = Vector(
//    "xxxxxxxxxxxxxxxxxx",
//    "x   x       x   ex",
//    "x   x    x  x xxxx",
//    "x        x  x    x",
//    "xs  x    x       x",
//    "xxxxxxxxxxxxxxxxxx")
//  println(solveMaze(maze)) // Some(rrurrrrruurrdddrrruuurrr)
}


