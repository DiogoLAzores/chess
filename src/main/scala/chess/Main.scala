package chess

import chess.BoardLogic._
import chess.models.King.isKingInCheck
import com.whitehatgaming.UserInputFile

import scala.annotation.tailrec

object Main extends App {
  val movesFile = new UserInputFile(
    "C:\\Users\\Utilizador\\Documents\\ChessProj\\chess\\data\\sample-moves.txt"
  )

  @tailrec
  private def play(
    wasLastMoveOk: Boolean = true,
    move: Option[Array[Int]] = Option(movesFile.nextMove()),
    lastMove: Option[Array[Int]] = None,
    currentPlayerInCheck: Boolean = false
  ): Unit = {
    println("#" * 30)
    println(s"Processing move: ${lastMove.fold("None - Initial Board State")(_.mkString("[", ", ", "]"))}")
    if (currentPlayerInCheck) println("[IN CHECK]") else ()

    move match {
      // No more moves to process
      case None =>
        presentBoard()
        println("No more moves - Final board state above")

      // Process new move
      case Some(Array(fColumn, fRow, tColumn, tRow)) =>
        val currentChessPiece = gameBoard(fRow)(fColumn)
        val isCurrentLight    = currentChessPiece.forall(_.isUpper)

        // Check if same player is doing next move by analyzing if new movement affects the same side of the board
        val samePlayer =
          lastMove.forall {
            case Array(_, _, oldTColumn, oldTRow) =>
              val previousChessPiece = gameBoard(oldTRow)(oldTColumn) // Uses destination square to identify
              val isEqualUpper       = previousChessPiece.forall(_.isUpper) && currentChessPiece.forall(_.isUpper)
              val isEqualLower       = previousChessPiece.forall(_.isLower) && currentChessPiece.forall(_.isLower)

              isEqualUpper || isEqualLower
          }

        // Play normally if either the last move was ok, or the same player is doing the current move
        if (wasLastMoveOk || samePlayer) {
          val outcome =
            // Check if movement exceeds board limits
            if (tColumn > gameBoard.length || tRow > gameBoard.length) {
              Left(s"Movement goes over board limits: $fColumn$fRow$tColumn$tRow")
            } else
              identifyChessPiece(gameBoard(fRow)(fColumn))
                .fold[Either[String, Board]](Left(s"No chess piece found in [$fColumn$fRow]"))(
                  _.move(gameBoard)((fColumn, fRow), (tColumn, tRow))
                )

          // Checks if current movement was successful
          val (isSuccessful, currentKingInCheck) = outcome match {
            case Left(error) =>
              println(error)
              (false, currentPlayerInCheck)
//            case Right(_) if isKingInCheck(isLight = isCurrentLight) =>
//              println(s"Player move failed since they are [IN CHECK]")
//              (false, isKingInCheck(isLight = isCurrentLight))
            case _ =>
              (true, false/*isKingInCheck(isLight = !isCurrentLight)*/)
          }

          presentBoard()

          // Send current outcome's result with the current move to next move's logic, moving to next player
          play(isSuccessful, lastMove = move/*, currentPlayerInCheck = currentKingInCheck*/)
        } else {
          // Send last outcome's result with last move to next move's logic, maintaining the player
          play(
            wasLastMoveOk,
            lastMove = lastMove/*,
            currentPlayerInCheck = isKingInCheck(isLight = isCurrentLight)*/
          )
        }

      case Some(_) => ()
    }
  }

  println("Starting game. Player 1 has light/UPPERCASE pieces, Player 2 has dark/lowercase ones")
  println("('Castling', 'En Passant' and 'Promotion' moves aren't considered since not required by test)")
  play()

}
