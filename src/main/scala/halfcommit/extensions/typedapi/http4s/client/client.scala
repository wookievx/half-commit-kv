package halfcommit.extensions.typedapi.http4s

import cats._
import cats.syntax.all._
import org.http4s.Status.Successful
import org.http4s.client.Client
import org.http4s._
import typedapi.client._

package object client {

  type RawHttp4sGet[F[_]] = RawGetRequest[Client[F], F] { type Resp = Response[F] }
  type RawHttp4sPut[F[_]] = RawPutRequest[Client[F], F] { type Resp = Response[F] }
  type RawHttp4sPutBd[F[_], Bd] = RawPutWithBodyRequest[Client[F], F, Bd] { type Resp = Response[F] }
  type RawHttp4sPost[F[_]] = RawPostRequest[Client[F], F] { type Resp = Response[F] }
  type RawHttp4sPostBd[F[_], Bd] = RawPostWithBodyRequest[Client[F], F, Bd] { type Resp = Response[F] }
  type RawHttp4sDelete[F[_]] = RawDeleteRequest[Client[F], F] { type Resp = Response[F] }

  private[client] implicit class Http4sRequestOps[F[_]](req: Request[F]) {

    def withQuery(queries: Map[String, List[String]]): Request[F] = {
      if (queries.nonEmpty) {
        val uri = req.uri =? queries
        req.withUri(uri)
      }
      else req
    }

    def withHeaders(headers: Map[String, String]): Request[F] = {
      if (headers.nonEmpty) {
        val h: List[Header] = headers.map { case (k, v) => org.http4s.Header(k, v) }(collection.breakOut)

        req.withHeaders(Headers(h))
      }
      else req
    }

    def run(cm: ClientManager[Client[F]])(implicit F: Applicative[F]): F[Response[F]] =
      cm.client.fetch(req)(resp => F.pure(resp))
  }


  private[client] def decodeFailedResponse[F[_], A](
    response: Response[F]
  )(implicit
    EF: MonadError[F, Throwable],
    asStr: EntityDecoder[F, String]
  ): F[A] = asStr.decode(response, strict = false).fold(
    error =>
      EF.raiseError[A](DecodeError("Failed to unmarhsal response", Some(error))),
    errorStr =>
      EF.raiseError[A](ResponseError(response.status.code, errorStr))
  ).flatten

  private[client] implicit class Http4sResponseOps[F[_]](private val resp: Response[F]) extends AnyVal {

    def decode[A](implicit
      d: EntityDecoder[F, A],
      asStr: EntityDecoder[F, String],
      F: MonadError[F, Throwable]
    ): F[A] = resp match {
      case Successful(_resp) =>
        d.decode(_resp, strict = false).fold(
          t => F.raiseError[A](DecodeError(t.message, Some(t))),
          F.pure
        ).flatten
      case failedResponse =>
        decodeFailedResponse(failedResponse)
    }
  }

  implicit def getRequest[F[_], A](implicit
    decoder: EntityDecoder[F, A],
    asStr: EntityDecoder[F, String],
    F: MonadError[F, Throwable],
    raw: RawHttp4sGet[F]
  ) = new GetRequest[Client[F], F, A] {

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], cm: ClientManager[Client[F]]): F[A] =
      F.flatMap(raw(uri, queries, headers, cm))(_.decode[A])
  }

  implicit def putRequest[F[_], A](implicit
    decoder: EntityDecoder[F, A],
    asStr: EntityDecoder[F, String],
    F: MonadError[F, Throwable],
    raw: RawHttp4sPut[F]
  ) = new PutRequest[Client[F], F, A] {

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], cm: ClientManager[Client[F]]): F[A] =
      F.flatMap(raw(uri, queries, headers, cm))(_.decode[A])
  }

  implicit def putBodyRequest[F[_], Bd, A](implicit
    encoder: EntityEncoder[F, Bd],
    decoder: EntityDecoder[F, A],
    asStr: EntityDecoder[F, String],
    F: MonadError[F, Throwable],
    raw: RawHttp4sPutBd[F, Bd]
  ) = new PutWithBodyRequest[Client[F], F, Bd, A] {

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], body: Bd, cm: ClientManager[Client[F]]): F[A] =
      F.flatMap(raw(uri, queries, headers, body, cm))(_.decode[A])
  }

  implicit def postRequest[F[_], A](implicit
    decoder: EntityDecoder[F, A],
    asStr: EntityDecoder[F, String],
    F: MonadError[F, Throwable],
    raw: RawHttp4sPost[F]
  ) = new PostRequest[Client[F], F, A] {

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], cm: ClientManager[Client[F]]): F[A] =
      F.flatMap(raw(uri, queries, headers, cm))(_.decode[A])
  }

  implicit def postBodyRequest[F[_], Bd, A](implicit
    encoder: EntityEncoder[F, Bd],
    decoder: EntityDecoder[F, A],
    asStr: EntityDecoder[F, String],
    F: MonadError[F, Throwable],
    raw: RawHttp4sPostBd[F, Bd]
  ) = new PostWithBodyRequest[Client[F], F, Bd, A] {

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], body: Bd, cm: ClientManager[Client[F]]): F[A] =
      F.flatMap(raw(uri, queries, headers, body, cm))(_.decode[A])
  }

  implicit def deleteRequest[F[_], A](implicit
    decoder: EntityDecoder[F, A],
    asStr: EntityDecoder[F, String],
    F: MonadError[F, Throwable],
    raw: RawHttp4sDelete[F]
  ) = new DeleteRequest[Client[F], F, A] {

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], cm: ClientManager[Client[F]]): F[A] =
      F.flatMap(raw(uri, queries, headers, cm))(_.decode[A])
  }



}
