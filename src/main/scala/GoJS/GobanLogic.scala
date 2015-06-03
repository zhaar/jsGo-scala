package GoJS

object GobanLogic {

  trait Stone

  case object White extends Stone

  case object Black extends Stone

  type Board = Array[Array[Option[Stone]]]

  case class BoardState(size: Int, board: Board, currentPlayer: Stone = Black, history: List[Int] = Nil)

  implicit class array2d(board: Board) {
    def access(x: Int, y: Int): Option[Option[Stone]] = {
      if (0 <= x && x < board.length && 0 <= y && y < board.length) {
        Some(board(x)(y))
      } else {
        None
      }
    }
  }


  def emptyBoard(size: Int): Board = {
    def emptyArray(): Array[Option[Stone]] = {
      (0 until size).toArray.map { j => None }
    }
    (0 until size).toArray.map { i => emptyArray() }
  }

  def wantToPlayOn(x: Int, y: Int, gameState: BoardState): BoardState = {
    println(s"-----NEW STONE: ${gameState.currentPlayer} on $x, $y-----")
    val stone = gameState.board.access(x, y)
    stone match {
      case Some(None) => PlayOn(x, y, gameState)
      case _ => {
        println(s"Can't play on $x, $y")
        gameState
      }
    }
  }

  def PlayOn(x: Int, y: Int, gameState: BoardState): BoardState = {
    val newBoard = gameState.board
    val color = gameState.currentPlayer

    newBoard(x)(y) = Some(color)
    val currentGroup = getGroup(x:Int, y: Int, newBoard)
    val enemyGroups = ennemyNeighbors(x, y, newBoard, color).map { case (a, b) => getGroup(a, b, newBoard)}.toSet
    if (!isGroupAlive(currentGroup, newBoard)) {
      println(s"group on $x, $y is dead")
      if (!enemyGroups.forall(ls => isGroupAlive(ls, newBoard))){
        val toRemove = enemyGroups.filterNot { ls => isGroupAlive(ls, newBoard) }.flatten
        toRemove.foreach{ case (a, b) => newBoard(a)(b) = None}
        BoardState(gameState.size, newBoard, opposite(color))
      } else {
        println("can't suicide")
        newBoard(x)(y) = None //this mutable state screws up everything
        gameState
      }
    } else if(isKo(newBoard, gameState)){
      gameState
    } else {
      val toRemove = enemyGroups.filterNot { ls => isGroupAlive(ls, newBoard) }.flatten
      toRemove.foreach{ case (a, b) => newBoard(a)(b) = None}
      BoardState(gameState.size, newBoard, opposite(color))
    }
  }

  def collectDeadStones(x: Int, y: Int, board: Board): List[Stone] = Nil

  def doesKill(x: Int, y: Int, board: Board): Boolean = {
    val n = friendlyNeighbors(x, y, board, opposite(board(x)(y).get))
    !n.forall {case (a: Int, b:Int) => isGroupAlive(a,b,board)}
  }

  def isKo(newBoard: Board, state: BoardState) = false

  def opposite(color: Stone): Stone = color match {
    case White => Black
    case Black => White
  }

  def rawNeighbors(x: Int, y: Int): List[(Int, Int)] = {
    (x + 1, y) :: (x - 1, y) :: (x, y + 1) :: (x, y - 1) :: Nil
  }

  def filterNeighbors(x: Int, y: Int, board: Board)(fn: Option[Option[Stone]] => Boolean): List[(Int, Int)] = {
    rawNeighbors(x, y).filter { c => fn(board.access(c._1, c._2))}
  }

  def directNeighbors(x: Int, y: Int, board: Board): List[(Int, Int)] =
    filterNeighbors(x, y, board) {
      case Some(Some(stone)) => true
      case _ => false
    }

  def friendlyNeighbors(x: Int, y: Int, board: Board, color: Stone): List[(Int, Int)] =
    filterNeighbors(x, y, board) {
      case Some(Some(stone)) => stone == color
      case _ => false
    }

  def ennemyNeighbors(x: Int, y: Int, board: Board, color: Stone): List[(Int, Int)] =
    friendlyNeighbors(x,y, board, opposite(color))

  def freeNeighbords(x: Int, y: Int, board: Board): List[(Int, Int)] =
    filterNeighbors(x, y, board) {
      case Some(None) => true
      case _ => false
    }

  def stoneLiberties(x: Int, y: Int, board: Board): Int = freeNeighbords(x, y, board).length

  def groupLiberties(x: Int, y: Int, board: Board): Int = {
    println(s"checking group liberties from stone $x, $y")
    val s = getGroup(x, y, board).map {
      case (a, b) => stoneLiberties(a, b, board)
    }.sum
    println(s"group has $s liberties (with duplicates)");s
  }

  def groupLiberties(ls: List[(Int, Int)], board: Board): Int = ls.map { case (x,y) => stoneLiberties(x,y,board)}.sum

  def isGroupAlive(x: Int, y: Int, board: Board): Boolean = {
    println(s"Checking group alive on $x, $y")
    groupLiberties(x, y, board) > 0
  }

  def isGroupAlive(ls: List[(Int, Int)], board: Board): Boolean = groupLiberties(ls, board) > 0

  def getGroup(x: Int, y: Int, board: Board): List[(Int, Int)] = {
    println(s"retreiving group starting at stone $x, $y")
    def reccursiveCheck(x: Int, y: Int, acc: List[(Int, Int)], toCheck:List[(Int,Int)]): List[(Int, Int)] = {
      val alliesToCheck = friendlyNeighbors(x, y, board, board(x)(y).get).filter { n =>
        !acc.contains(n) && !toCheck.contains(n)
      }
      val left2check = toCheck ::: alliesToCheck
      left2check match {
        case Nil => acc
        case (a, b) :: rest => reccursiveCheck(a, b, (a,b) :: acc, rest )
      }
    }
    reccursiveCheck(x, y, (x,y)::Nil, Nil)
  }

}