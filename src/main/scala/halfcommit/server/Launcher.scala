package halfcommit.server

import cats.effect._
import cats.syntax.all._
import halfcommit.extensions.typedapi.http4s.server._
import halfcommit.api
import io.circe.Json
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.server.blaze.BlazeServerBuilder
import typedapi.server._


object Launcher extends IOApp {

  def ????[T]: IO[T] = IO.raiseError(new NotImplementedError("Not implemented IO stub"))

  def get(id: String): IO[Result[Json]] =
    IO.pure(successWith(SuccessCode(200))(Json.obj("test" := "value")))
  def put(id: String, matching: String, value: Json): IO[Result[Json]] = ????
  def post(id: String, etag: String, value: Json): IO[Result[Json]] = ????
    IO.pure(success(Json.fromString("OK")).asRight[HttpError])
  def delete(id: String, matching: String): IO[Result[Json]] = ????

  def run(args: List[String]): IO[ExitCode] = {

    val endpoints = deriveAll[IO](api.endpoint[Json, String]).from(get, put, post, delete)
    val sm = ServerManager(BlazeServerBuilder[IO], "0.0.0.0", 8080)

    mount(sm, endpoints)
  }

}
