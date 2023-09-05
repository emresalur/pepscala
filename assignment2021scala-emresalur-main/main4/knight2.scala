// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object M4b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
    val legalPath = path.find(yCord => yCord == x).isDefined
    val insideBoard = (x._1 < dim) && (x._2 < dim) && (x._1 >= 0) && (x._2 >= 0)
    (!legalPath && insideBoard)
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
   val a = (x._1+1, x._2+2);
   val b = (x._1+2, x._2+1);
   val c = (x._1+2, x._2-1);
   val d = (x._1+1, x._2-2);
   val e = (x._1-1, x._2-2);
   val f = (x._1-2, x._2-1);
   val g = (x._1-2, x._2+1);
   val h = (x._1-1, x._2+2)
   val legalMovesList = List(a,b,c,d,e,f,g,h).filter(currentMove => is_legal(dim, path, currentMove))
   legalMovesList
}

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
  if (xs.size == 0) {
    None
  }
  else {
    val fx = f(xs.head)
    if (fx != None) {
      fx
    } 
    else {
      first(xs.drop(1), f)
    }
  }
}

//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val ordered_possible_moves = legal_moves(dim, path, x)
    ordered_possible_moves.sortWith(legal_moves(dim, path, _).size < legal_moves(dim, path, _).size)
}


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 00**********************************************


def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    if (path.length == dim * dim && (legal_moves(dim, List(path.head), path.head).contains(path.last))) {
        Some(path)
    } 
    else {
        first(ordered_moves(dim, path, path.head),
        a => first_closed_tour_heuristics(dim, a :: path))
    }
}


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = ???



}
