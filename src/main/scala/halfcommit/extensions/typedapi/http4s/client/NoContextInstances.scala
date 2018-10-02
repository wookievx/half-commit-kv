package halfcommit.extensions.typedapi.http4s.client

import cats.{Applicative, Monad}
import org.http4s._
import org.http4s.client.Client
import typedapi.client._

trait NoContextInstances {

  implicit def rawGetRequestNoCtx[F[_]](implicit F: Applicative[F]) = new RawGetRequest[Client[F], F] {
    type Resp = Response[F]

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], cm: ClientManager[Client[F]]): F[Resp] = {
      val request = Request[F](Method.GET, Uri.unsafeFromString(deriveUriString(cm, uri)))
        .withQuery(queries)
        .withHeaders(headers)

      request.run(cm)
    }
  }

  implicit def rawPutRequestNoCtx[F[_]](implicit F: Applicative[F]) = new RawPutRequest[Client[F], F] {
    type Resp = Response[F]

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], cm: ClientManager[Client[F]]): F[Resp] = {
      val request = Request[F](Method.PUT, Uri.unsafeFromString(deriveUriString(cm, uri)))
        .withQuery(queries)
        .withHeaders(headers)

      request.run(cm)
    }
  }

  implicit def rawPutBodyRequestNoCtx[F[_], Bd](implicit encoder: EntityEncoder[F, Bd], F: Monad[F]) = new RawPutWithBodyRequest[Client[F], F, Bd] {
    type Resp = Response[F]

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], body: Bd, cm: ClientManager[Client[F]]): F[Resp] = {
      val request = Request[F](Method.PUT, Uri.unsafeFromString(deriveUriString(cm, uri)))
        .withQuery(queries)
        .withHeaders(headers)
        .withEntity(body)

      request.run(cm)
    }
  }

  implicit def rawPostRequestNoCtx[F[_]](implicit F: Applicative[F]) = new RawPostRequest[Client[F], F] {
    type Resp = Response[F]

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], cm: ClientManager[Client[F]]): F[Resp] = {
      val request = Request[F](Method.POST, Uri.unsafeFromString(deriveUriString(cm, uri)))
        .withQuery(queries)
        .withHeaders(headers)

      request.run(cm)
    }
  }

  implicit def rawPostBodyRequestNoCtx[F[_], Bd](implicit
    encoder: EntityEncoder[F, Bd],
    F: Monad[F],
  ) = new RawPostWithBodyRequest[Client[F], F, Bd] {
    type Resp = Response[F]

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], body: Bd, cm: ClientManager[Client[F]]): F[Resp] = {
      val request = Request[F](Method.POST, Uri.unsafeFromString(deriveUriString(cm, uri)))
        .withQuery(queries)
        .withHeaders(headers)
        .withEntity(body)

      request.run(cm)
    }
  }

  implicit def rawDeleteRequestNoCtx[F[_]](implicit F: Applicative[F]) = new RawDeleteRequest[Client[F], F] {
    type Resp = Response[F]

    def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], cm: ClientManager[Client[F]]): F[Resp] = {
      val request = Request[F](Method.DELETE, Uri.unsafeFromString(deriveUriString(cm, uri)))
        .withQuery(queries)
        .withHeaders(headers)

      request.run(cm)
    }
  }


}