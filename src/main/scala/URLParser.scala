package typelevel

import scala.compiletime.ops.string._
import cats.implicits._
import cats.effect._

object URLParser extends IOApp:

  sealed trait URLComponent

  sealed trait Protocol extends URLComponent
  case object HTTP extends Protocol
  case object HTTPS extends Protocol

  type DomainName[S <: String]
  type URLPath[S <: String]

  type URLRest[L <: String, R <: String] <: Tuple =
    R match
      case "" => EmptyTuple
      case _  =>
        CharAt[R, 0] match
          case '/' => (DomainName[L], URLPath[Substring[R, 1, Length[R]]])
          case _ => URLRest[L + Substring[R, 0, 1], Substring[R, 1, Length[R]]]

  type URLComponents[S <: String] <: Tuple =
    S match
      case "" => EmptyTuple
      case _ =>
        Matches[S, "^https:.*"] match
          case true => HTTPS.type *: URLRest["", Substring[S, 8, Length[S]]]
          case false => HTTP.type *: URLRest["", Substring[S, 8, Length[S]]]

  // private def fakeQueryURL(url: String)(f: URLComponents[url.type] => IO[Unit]): IO[Unit] = {
  //   f(url)
  // }

  def run(args: List[String]): IO[ExitCode] = {
    summon[URLComponents["https://kek.com/foo/bar"] =:= (HTTPS.type, DomainName["kek.com"], URLPath["foo/bar"])]
    IO.println("test")
      .as(ExitCode.Success)
  }

