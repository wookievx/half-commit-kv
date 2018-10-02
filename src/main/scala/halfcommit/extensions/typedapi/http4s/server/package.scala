package halfcommit.extensions.typedapi.http4s

import cats.MonadError
import cats.effect._
import cats.syntax.all._
import io.circe.Encoder
import org.http4s._
import org.http4s.circe.jsonEncoderOf
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.EntityResponseGenerator
import org.http4s.server.blaze.BlazeServerBuilder
import shapeless._
import shapeless.ops.hlist._
import typedapi.server._
import typedapi.shared._

package object server extends PrimitiveExtractors with HeaderExtractors {

  private def responseGen[F[_]](_status: Status) = new EntityResponseGenerator[F] { val status: Status = _status }

  implicit val errorEncoder: Encoder[HttpError] = {
    import io.circe.generic.semiauto._
    implicit val encodeCode: Encoder[ErrorCode] = Encoder.encodeInt.contramap(_.statusCode)
    val _ = encodeCode
    deriveEncoder[HttpError]
  }

  implicit def encodeHttpError[F[_]: Sync]: EntityEncoder[F, HttpError] = jsonEncoderOf[F, HttpError]

  private class StatusParseError(parseCause: Throwable, cause: Option[Throwable]) extends
    Throwable(parseCause.getMessage, parseCause)

  private def getHeaders(raw: Map[String, String]): List[Header.Raw] =
    raw.map { case (key, value) => Header(key, value) }(collection.breakOut)

  implicit def noReqBodyExecutor[El <: HList, KIn <: HList, VIn <: HList, M <: MethodType, F[_], FOut](implicit
    encoder: EntityEncoder[F, FOut],
    errorEncoder: EntityEncoder[F, HttpError],
    ME: MonadError[F, Throwable]
  ) =
    new NoReqBodyExecutor[El, KIn, VIn, M, F, FOut] {
      type R = Request[F]
      type Out = F[Response[F]]

      private val dsl = Http4sDsl[F]

      import dsl._

      def apply(req: R, eReq: EndpointRequest, endpoint: Endpoint[El, KIn, VIn, M, VIn, F, FOut]): Either[ExtractionError, Out] = {
        extract(eReq, endpoint).map { extracted =>
          execute(extracted, endpoint) flatMap {
            case Right((code, response)) =>
              responseGen[F](Status(code.statusCode))(response, getHeaders(endpoint.headers):_*)
            case Left(error) =>
              responseGen[F](Status(error.code.statusCode))(error.message, getHeaders(endpoint.headers):_*)
          } handleErrorWith { t =>
              InternalServerError(t.getMessage)
          }
        }
      }
    }

  implicit def withReqBodyExecutor[El <: HList, KIn <: HList, VIn <: HList, Bd, M <: MethodType, ROut <: HList, POut <: HList, F[_], FOut]
  (implicit
    encoder: EntityEncoder[F, FOut],
    errorEncoder: EntityEncoder[F, HttpError],
    decoder: EntityDecoder[F, Bd],
    ME: MonadError[F, Throwable],
    _prepend: Prepend.Aux[ROut, Bd :: HNil, POut],
    _eqProof: POut =:= VIn) = new ReqBodyExecutor[El, KIn, VIn, Bd, M, ROut, POut, F, FOut] {
    type R = Request[F]
    type Out = F[Response[F]]

    implicit val prepend = _prepend
    implicit val eqProof = _eqProof

    private val dsl = Http4sDsl[F]

    import dsl._

    def apply(req: R, eReq: EndpointRequest, endpoint: Endpoint[El, KIn, VIn, M, (BodyType[Bd], ROut), F, FOut]): Either[ExtractionError, Out] = {
      extract(eReq, endpoint) map { case (_, extracted) =>
        val successCode = for {
          body <- req.as[Bd]
          responseEither <- execute(extracted, body, endpoint)
        } yield responseEither
        successCode flatMap {
          case Right((code, response)) =>
            responseGen[F](Status(code.statusCode))(response, getHeaders(endpoint.headers):_*)
          case Left(error) =>
            responseGen[F](Status(error.code.statusCode))(error.message, getHeaders(endpoint.headers):_*)
        } handleErrorWith { t =>
            InternalServerError(t.getMessage)
        }
      }
    }
  }

  implicit def mountEndpoints[F[_]](implicit
    S: Sync[F]
  ) = new MountEndpoints[BlazeServerBuilder[F], Request[F], F[Response[F]]] {
    val dsl = Http4sDsl[F]

    import dsl._

    type Out = F[ExitCode]

    def apply(server: ServerManager[BlazeServerBuilder[F]], endpoints: List[Serve[Request[F], F[Response[F]]]]): Out = {
      val service = HttpRoutes.of[F] {
        case request =>
          def execute(eps: List[Serve[Request[F], F[Response[F]]]], eReq: EndpointRequest): F[Response[F]] = eps match {
            case collection.immutable.::(endpoint, tail) => endpoint(request, eReq) match {
              case Right(response) => response
              case Left(RouteNotFound) => execute(tail, eReq)
              case Left(BadRouteRequest(msg)) => BadRequest(msg)
            }

            case Nil => NotFound("uri = " + request.uri)
          }

          val eReq = EndpointRequest(
            request.method.name,
            {
              val path = request.uri.path.split("/")

              if (path.isEmpty) List.empty
              else path.tail.toList
            },
            request.uri.multiParams.map { case (key, value) => key -> value.toList },
            request.headers.toList.map(header => header.name.toString.toLowerCase -> header.value)(collection.breakOut)
          )

          if (request.method.name == "OPTIONS") {
            S.delay(Response(headers = Headers(getHeaders(optionsHeaders(endpoints, eReq)))))
          }
          else
            execute(endpoints, eReq)
      }

      server.server.bindHttp(server.port, server.host).withHttpApp(service.orNotFound).serve
        .compile
        .drain
        .as(ExitCode.Success)
    }
  }

}
