package com.github.nbenns.shared

import cats.{ApplicativeError, MonadError}
import cats.implicits._

import scala.language.higherKinds

object Conversions {
  def strToLong[F[_, _]](str: String)
  (implicit apError: ApplicativeError[F[NumberFormatException, *], NumberFormatException]): F[NumberFormatException, Long] =
    if (str.forall(c => c.isDigit || c == '-')) str.toLong.pure[F[NumberFormatException, *]]
    else (new NumberFormatException).raiseError[F[NumberFormatException, *], Long]

  def longToInt[F[_, _]](l: Long)
  (implicit apError: ApplicativeError[F[NumberFormatException, *], NumberFormatException]): F[NumberFormatException, Int] =
    if (l >= Int.MinValue && l <= Int.MaxValue) l.toInt.pure[F[NumberFormatException, *]]
    else (new NumberFormatException(l.toString)).raiseError[F[NumberFormatException, *], Int]

  def strToInt[F[_, _]](s: String)
  (implicit monadError: MonadError[F[NumberFormatException, *], NumberFormatException]): F[NumberFormatException, Int] =
    strToLong[F](s).flatMap(longToInt[F])
}
