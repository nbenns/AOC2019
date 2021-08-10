package com.github.nbenns.shared

object Conversions {
  def strToLong(str: String): Either[NumberFormatException, Long] =
    if (str.forall(c => c.isDigit || c == '-')) Right(str.toLong)
    else Left(new NumberFormatException)

  def longToInt(l: Long): Either[NumberFormatException, Int] =
    if (l >= Int.MinValue && l <= Int.MaxValue) Right(l.toInt)
    else Left(new NumberFormatException(l.toString))

  def strToInt(s: String): Either[NumberFormatException, Int] =
    strToLong(s).flatMap(longToInt)
}
