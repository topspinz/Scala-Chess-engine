import scala.collection.immutable._
import scala.io.StdIn
/* 

//Scala chess engine

----> To run program type: 1. scalac a6.scala    2. scala Chess

Example Inputs: a2 a3
				a3 a4
				b1 b3.. ect....





*/

/*

	Gets the abstract type for the color basically defines the pieces 


*/

abstract class PieceColor
  case object White extends PieceColor 
  case object Black extends PieceColor

abstract class PieceType
  case object Pawn extends PieceType
  case object Knight extends PieceType
  case object Bishop extends PieceType
  case object Rook extends PieceType
  case object Queen extends PieceType
  case object King extends PieceType
 
/*
	Basic data array structure for the chessBoard displays the piecetype with its appropriate color
*/
object Chess {
	def main(args: Array[String]) {
		def newChessBoard(): Board = {
			new Board(Array(
							Array(new GetPiece("rook", "black"), new GetPiece("knight", "black"), new GetPiece("bishop", "black"), new GetPiece("queen", "black"), new GetPiece("king", "black"), new GetPiece("bishop", "black"), new GetPiece("knight", "black"), new GetPiece("rook", "black")),
							Array(new GetPiece("pawn", "black"), new GetPiece("pawn", "black"), new GetPiece("pawn", "black"), new GetPiece("pawn", "black"), new GetPiece("pawn", "black"), new GetPiece("pawn", "black"), new GetPiece("pawn", "black"), new GetPiece("pawn", "black")),
							Array(new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none")),
							Array(new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none")),
							Array(new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none")),
							Array(new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none"), new GetPiece("empty", "none")),
							Array(new GetPiece("pawn", "white"), new GetPiece("pawn", "white"), new GetPiece("pawn", "white"), new GetPiece("pawn", "white"), new GetPiece("pawn", "white"), new GetPiece("pawn", "white"), new GetPiece("pawn", "white"), new GetPiece("pawn", "white")),
							Array(new GetPiece("rook", "white"), new GetPiece("knight", "white"), new GetPiece("bishop", "white"), new GetPiece("queen", "white"), new GetPiece("king", "white"), new GetPiece("bishop", "white"), new GetPiece("knight", "white"), new GetPiece("rook", "white"))
						))
		}

		def toPos(cor: Char): Int = {
			val coordinate: Char = cor
			val caseMatches = 20
			val movedelSet= caseMatches * 2
			coordinate match {
				case 'a' => 0 case 'b' => 1 case 'c' => 2 case 'd' => 3 case 'e' => 4 case 'f' => 5 case 'g' => 6 case 'h' => 7	case '1' => 7 case '2' => 6 case '3' => 5 case '4' => 4 case '5' => 3 case '6' => 2 case '7' => 1 case '8' => 0
				case _ => 8
			}
		
			
		}
		
		def GetUserInput(input: String, brd: Board) {
			val aBoard: Board = brd
			val boardPieces = 8
			aBoard.move(toPos(input.charAt(0)), toPos(input.charAt(1)), toPos(input.charAt(3)), toPos(input.charAt(4)))
		}

		def getInput(brd: Board) {
			val theBoard: Board = brd
			print("Input example ( a1 b2 ) (without brackets) this will move piece at a1 to b2 if possible  ")
			var line = StdIn.readLine()
			while (line != "finish") {
				GetUserInput(line, theBoard)
				theBoard.printChessBoard()
				line = StdIn.readLine()
			}
		}

		val chessBoard = newChessBoard()
		chessBoard.printChessBoard()
		getInput(chessBoard)
	}
}

/*
	Gets the pieces for the Board and checks if the moves are possible
*/
class Board(bd: Array[Array[GetPiece]]) {
	val theBoard: Array[Array[GetPiece]] = bd
	
	def getBoard(): Array[Array[GetPiece]] = theBoard

	def getGetPiece(x: Int, y: Int): GetPiece = {
		theBoard(y)(x)
	}

	def BoardStae(xi: Int, yi: Int, xf: Int, yf: Int): Array[Array[GetPiece]] = {
		val initialGetPiece: GetPiece = theBoard(yi)(xi)
		theBoard(yi)(xi) = new GetPiece("empty", "none")
		theBoard(yf)(xf) = initialGetPiece
		theBoard
	}

	def possibleMove(xi: Int, yi: Int, xf: Int, yf: Int): Boolean = {
		val initialGetPiece: GetPiece = theBoard(yi)(xi)
		
		if (initialGetPiece.PossibleMoveSet(xi, yi, xf, yf)) {
			if (xf < 8 && xf > -1 && yf < 8 && yf > -1) { 
				val finalGetPiece: GetPiece = theBoard(yf)(xf)
				if (initialGetPiece.getType() == "empty" || initialGetPiece.getColour() != finalGetPiece.getColour()) {
					return true
				} else {
					return false
					}
				return true 
			} else {
				return false 
				}
			return true 
		} else {
			return false
		}
	}

	def move(xi: Int, yi: Int, xf: Int, yf: Int) {
		if (possibleMove(xi, yi, xf, yf)) BoardStae(xi, yi, xf, yf)
		else println("Error invalid move")
	}

	def printChessBoard() {
		var posx: Int = 8
		var posy: Int = 8
		var x: Int = 0
		var y: Int = 0
		printf("\n")
		while(y < 8) {
			while(x < 8) {
				theBoard(y)(x).printPiece()
				print(" ") 
				x += 1
			}
			printf("\n")
			x = 0
			y += 1
		}
		printf("\n")
	}
}

/*
	Moves the pieces based on the coordinates given to them by the rules of the chessboard
*/
class GetPiece(tp: String, cl: String) {
	var piecetype: String = tp
	var colour: String = cl
	def PossibleMoveSet (w: Int, x: Int, y: Int, z: Int): Boolean = {
		val check = (y - w, z - x)
		piecetype match {
			case "empty" => false
			case "pawn" => check match {
				case (0, 1) => true case (0, -1) => true case _ => false
			}
			case "rook" => check match {
				case (0, 7) => true case (0, 6) => true case (0, 5) => true case (0, 4) => true case (0, 3) => true case (0, 2) => true case (0, 1) => true case (0, -7) => true case (0, -6) => true case (0, -5) => true case (0, -4) => true case (0, -3) => true case (0, -2) => true case (0, -1) => true case (7, 0) => true case (6, 0) => true case (5, 0) => true case (4, 0) => true case (3, 0) => true case (2, 0) => true case (1, 0) => true case _ => false
			}
			case "bishop" => check match {
				case (7, 7) => true case (6, 6) => true case (5, 5) => true case (4, 4) => true case (3, 3) => true case (2, 2) => true case (1, 1) => true case (7, -7) => true case (6, -6) => true case (5, -5) => true case (4, -4) => true case (3, -3) => true case (2, -2) => true case (1, -1) => true case (-7, 7) => true case (-6, 6) => true case (-5, 5) => true case (-4, 4) => true case (-3, 3) => true case (-2, 2) => true case (-1, 1) => true case (-7, -7) => true 	case _ => false 
			}
			case "queen" => check match {
				case (0, 7) => true case (0, 6) => true case (0, 5) => true case (0, 4) => true case (0, 3) => true case (0, 2) => true case (0, 1) => true case (0, -7) => true case (0, -6) => true case (0, -5) => true case (0, -4) => true case (0, -3) => true case (0, -2) => true case (0, -1) => true
				case (7, 0) => true case (6, 0) => true case (5, 0) => true case (4, 0) => true case (3, 0) => true case (2, 0) => true case (1, 0) => true case (-7, 0) => true case (-6, 0) => true case (-5, 0) => true case (-4, 0) => true case (-3, 0) => true case (-2, 0) => true case (-1, 0) => true case (7, 7) => true case (6, 6) => true case (5, 5) => true case (4, 4) => true case (3, 3) => true case (2, 2) => true case (1, 1) => true
				case (7, -7) => true case (6, -6) => true case (5, -5) => true case (4, -4) => true case (3, -3) => true case (2, -2) => true case (1, -1) => true case (-7, 7) => true case (-6, 6) => true case (-5, 5) => true case (-4, 4) => true case (-3, 3) => true case (-2, 2) => true;
				case _ => false
			}
			case "king" => check match {
				case (0, 1) => true case (0, -1) => true case (1, 0) => true case (-1, 0) => true case (1, 1) => true case (1, -1) => true case (-1, 1) => true case (-1, -1) => true 
				case _ => false
			}
			case "knight" => check match {
				case (2, 1) => true case (1, 2) => true case (2, -1) => true case (-1, 2) => true case (1, -2) => true case (-2, 1) => true case (-2, -1) => true case (-1, -2) => true
				case _ => false
			}
			
			case "NNW" => check match {
			case (0, 1) => true case (0, -1) => true case _ => false
			}
			case "NNE" => check match {
				case (0, 7) => true case (0, 6) => true case (0, 5) => true case (0, 4) => true case (0, 3) => true case (0, 2) => true case (0, 1) => true case (0, -7) => true case (0, -6) => true case (0, -5) => true case (0, -4) => true case (0, -3) => true case (0, -2) => true case (0, -1) => true case (7, 0) => true case (6, 0) => true case (5, 0) => true case (4, 0) => true case (3, 0) => true case (2, 0) => true case (1, 0) => true case _ => false
			}
			case "NNS" => check match {
				case (7, 7) => true case (6, 6) => true case (5, 5) => true case (4, 4) => true case (3, 3) => true case (2, 2) => true case (1, 1) => true case (7, -7) => true case (6, -6) => true case (5, -5) => true case (4, -4) => true case (3, -3) => true case (2, -2) => true case (1, -1) => true case (-7, 7) => true case (-6, 6) => true case (-5, 5) => true case (-4, 4) => true case (-3, 3) => true case (-2, 2) => true case (-1, 1) => true case (-7, -7) => true 	case _ => false 
			}
			case "EWE" => check match {
				case (0, 7) => true case (0, 6) => true case (0, 5) => true case (0, 4) => true case (0, 3) => true case (0, 2) => true case (0, 1) => true case (0, -7) => true case (0, -6) => true case (0, -5) => true case (0, -4) => true case (0, -3) => true case (0, -2) => true case (0, -1) => true
				case (7, 0) => true case (6, 0) => true case (5, 0) => true case (4, 0) => true case (3, 0) => true case (2, 0) => true case (1, 0) => true case (-7, 0) => true case (-6, 0) => true case (-5, 0) => true case (-4, 0) => true case (-3, 0) => true case (-2, 0) => true case (-1, 0) => true case (7, 7) => true case (6, 6) => true case (5, 5) => true case (4, 4) => true case (3, 3) => true case (2, 2) => true case (1, 1) => true
				case (7, -7) => true case (6, -6) => true case (5, -5) => true case (4, -4) => true case (3, -3) => true case (2, -2) => true case (1, -1) => true case (-7, 7) => true case (-6, 6) => true case (-5, 5) => true case (-4, 4) => true case (-3, 3) => true case (-2, 2) => true;
				case _ => false
			}
			case "WWE" => check match {
				case (0, 1) => true case (0, -1) => true case (1, 0) => true case (-1, 0) => true case (1, 1) => true case (1, -1) => true case (-1, 1) => true case (-1, -1) => true 
				case _ => false
			}
			case "WWS" => check match {
				case (0, 1) => true case (0, -2) => true case (1, -1) => true case (-1, -2) => true case (1, -3) => true case (1, -5) => true case (-1, 6) => true case (-1, -1) => true 
				case _ => false
			}
			case "WWN" => check match {
				case (0, 1) => true case (0, -1) => true case (1, -2) => true case (-1, 5) => true case (1, 9) => true case (1, 2) => true case (-1, 2) => true case (-1, 3) => true 
				case _ => false
			}
			case "SSE" => check match {
				case (2, 1) => true case (1, 2) => true case (2, -1) => true case (-1, 2) => true case (1, -2) => true case (-2, 1) => true case (-2, -1) => true case (-1, -2) => true
				case _ => false
			}
			
			case "SSN" => check match {
				case (0, 1) => true case (1, 3) => true case (2, -1) => true case (-1, 5) => true case (1, 6) => true case (-2, 7) => true case (-2, 8) => true case (-1, 9) => true
				case _ => false
			}
			
			case "SSW" => check match {
				case (2, 1) => true case (1, 5) => true case (4, -1) => true case (5, 2) => true case (2, -2) => true case (1, 4) => true case (-2, -1) => true case (7, -2) => true
				case _ => false
			}
			
			case "NW" => check match {
				case (3, 1) => true case (3, -1) => true case (2, -2) => true case (-1, 8) => true case (1, 9) => true case (1, -4) => true case (-1, 8) => true case (-1, 6) => true 
				case _ => false
			}
			case "NE" => check match {
				case (2, 1) => true case (1, 2) => true case (2, -1) => true case (-1, 2) => true case (1, -2) => true case (-2, 1) => true case (-2, -1) => true case (-1, -2) => true
				case _ => false
			}
			
			case "NS" => check match {
				case (0, 1) => true case (1, 3) => true case (3, -1) => true case (-6, 3) => true case (5, 2) => true case (-2, 1) => true case (-2, -9) => true case (-1, 9) => true
				case _ => false
			}
			
			case "NN" => check match {
				case (2, 1) => true case (1, -2) => true case (4, -1) => true case (-5, -2) => true case (2, -2) => true case (-1, -1) => true case (-2, -1) => true case (5, 2) => true
				case _ => false
			}
				
			case _ => false
		}
			
			
		
	}

/*
	Prints the board appropriately
	*/
	def getColour(): String = colour
	def getType(): String = piecetype

	def printPiece() {
		colour match {
			case "black" => piecetype match {
				case "rook" => print("rb") case "knight" => print("nb") case "bishop" => print("bb") case "queen" => print("qb") case "king" => print("kb") case "pawn" => print("pb")
				case _ => print("--")
			}
			case "white" => piecetype match {
				case "rook" => print("rw") case "knight" => print("nw") case "bishop" => print("bw") case "queen" => print("qw") case "king" => print("kw") case "pawn" => print("pw")
				case _ => print("--")
			}
			case "none" => print("--") case _ => print("--")
		}
	}
}






