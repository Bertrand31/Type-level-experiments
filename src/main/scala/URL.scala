package typelevel

import scala.compiletime.ops.string.{CharAt, Length, Matches, Substring}
import cats.implicits._
import cats.effect._

object URL extends IOApp:

  sealed trait Protocol
  case object HTTP extends Protocol
  case object HTTPS extends Protocol

  type URLRest[S <: String] <: String

  type URLComponents[S <: String] <: Tuple =
    S match
      case "" => EmptyTuple
      case _ =>
        Matches[S, "^https:.*"] match
          case true => HTTPS.type *: Tuple1[Substring[S, 8, Length[S]]]
          case false => HTTP.type *: Tuple1[Substring[S, 8, Length[S]]]

  def run(args: List[String]): IO[ExitCode] = {
    summon[URLComponents["https://kek"] =:= (HTTPS.type, "kek")]
    IO.println("kek")
      .as(ExitCode.Success)
  }

