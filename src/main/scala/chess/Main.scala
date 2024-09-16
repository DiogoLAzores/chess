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
    lastMove.fold(print("Initial Board State"))(lMove => print(s"Processing move: ${lMove.mkString("[", ", ", "]")}"))

    move match {
      // No more moves to process
      case None =>
        println(if (currentPlayerInCheck) " (Next player ends up [IN CHECK])" else "")
        println("[No more moves]")
        presentBoard()

      // Process new move
      case Some(Array(fColumn, fRow, tColumn, tRow)) =>
        val oldGameBoard      = gameBoard
        val currentChessPiece = gameBoard(fRow)(fColumn)
        val isCurrentLight    = currentChessPiece.isUpper

        println(if (currentPlayerInCheck) s" (Current Player ${if (isCurrentLight) "1" else "2"} [IN CHECK])" else "")
        presentBoard()

        // Check if the current player is the same as last/last invalid move's player
        val samePlayer = {
          val checkPlayer = (oldRow: Int, oldColumn: Int) => {
            val previousChessPiece = gameBoard(oldRow)(oldColumn)
            val isEqualUpper       = previousChessPiece.isUpper && currentChessPiece.isUpper
            val isEqualLower       = previousChessPiece.isLower && currentChessPiece.isLower

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
            // Failed due to move error
            case Left(error) =>
              println(error)
              (false, currentPlayerInCheck)

            // Failed if own King is in check (board is reverted to not include movement)
            case Right(_) if isKingInCheck(isLight = isCurrentLight) =>
              println(s"Player move failed since they are [IN CHECK]")
              gameBoard = oldGameBoard
              (false, isKingInCheck(isLight = isCurrentLight))

            // Succeeded if own King is not in check
            case _ =>
              (true, isKingInCheck(isLight = !isCurrentLight))
          }

          // Send current outcome's result with the current move to next move's logic, moving to next player
          play(isSuccessful, lastMove = move, currentPlayerInCheck = currentKingInCheck)
        } else {
          // Send last outcome's result with last move to next move's logic, maintaining the player
          play(
            wasLastMoveOk,
            lastMove = lastMove,
            currentPlayerInCheck = isKingInCheck(isLight = isCurrentLight)
          )
        }

      case Some(_) => ()
    }
  }

  println("Starting game. Player 1 has light/UPPERCASE pieces, Player 2 has dark/lowercase ones")
  println("('Castling', 'En Passant' and 'Promotion' moves aren't considered since not required by test)")
  play()

}
