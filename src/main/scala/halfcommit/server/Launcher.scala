package halfcommit.server

import cats.effect._
import cats.syntax.all._
import halfcommit.extensions.typedapi.http4s.server._
import halfcommit.api
import halfcommit.api.{ETagQuery, EntityTag}
import io.circe.Json
import io.circe.syntax._
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.circe._
import org.http4s.server.blaze.BlazeServerBuilder
import typedapi.server._

object Launcher extends IOApp {

  def success[T](v: T): (SuccessCode, T) = SuccessCode(200) -> v

  def get(id: String): IO[Result[Json]] =
    IO.pure(success(Json.obj("test" := "value")).asRight[HttpError])

  def getDebug(id: String, matching: ETagQuery): IO[Result[Json]] =
    get(id)

  def put(id: String, matching: ETagQuery, value: Json): IO[Result[Json]] = ???
  def putRaw(id: String, value: Json): IO[Result[Json]] = ???
  def post(id: String, etag: EntityTag, value: Json): IO[Result[Json]] =
    IO.pure(success(Json.fromString("OK")).asRight[HttpError])
  def delete(id: String, matching: ETagQuery): IO[Result[Json]] = put(id, matching, Json.Null)


  def run(args: List[String]): IO[ExitCode] = {

    val endpoints = derive[IO](api.putEndpoint[Json, String]).from(put)
    val sm = ServerManager(BlazeServerBuilder[IO], "0.0.0.0", 8080)

    implicitly[EntityEncoder[IO, HttpError]]
    implicitly[EntityEncoder[IO, Json]]
    implicitly[EntityDecoder[IO, Json]]
    mount(sm, endpoints)
  }

}
