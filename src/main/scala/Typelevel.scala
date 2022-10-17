package typelevel

import scala.compiletime.ops.string.{CharAt, Length, Substring}
import cats.implicits._
import cats.effect._

object Typelevel extends IOApp:

  type ArgTypes[S <: String] <: Tuple =
    S match
      case "" => EmptyTuple
      case _ => CharAt[S, 0] match
        case '%' => CharAt[S, 1] match
          case 's' => String *: ArgTypes[Substring[S, 2, Length[S]]]
          case 'd' => Int *: ArgTypes[Substring[S, 2, Length[S]]]
        case _ => ArgTypes[Substring[S, 1, Length[S]]]

  private def printParser[T <: Tuple](pattern: List[Char], args: T, soFar: String = ""): String =
    pattern match
      case '%' :: _ :: rest =>
        args match
          case (arg: String) *: restArgs => printParser(rest, restArgs, soFar ++ arg)
          case (arg: Int) *: restArgs    => printParser(rest, restArgs, soFar ++ arg.toString)
          case _                         => throw new Error("Not enough arguments")
      case char :: rest => printParser(rest, args, soFar :+ char)
      case Nil          => soFar

  private def printf(pattern: String)(params: ArgTypes[pattern.type]): IO[Unit] =
    IO.println(printParser(pattern.toList, params))

  def run(args: List[String]): IO[ExitCode] =
    this.printf("%s is %d")("Ada", 36)
      .as(ExitCode.Success)

