package chess.check

object Check {
  def calculateBound(kingPos: Int, isUpper: Boolean): Int = {
    if (isUpper) Math.min(kingPos + 1, 7) else Math.max(kingPos - 1, 0)
  }
}
