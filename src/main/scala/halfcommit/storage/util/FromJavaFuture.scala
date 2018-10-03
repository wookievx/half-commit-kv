package halfcommit.storage.util

import java.util.concurrent.{CompletableFuture, CompletionException}

import cats.effect.Effect
import cats.syntax.all._

import scala.reflect.ClassTag
import scala.util.control.NonFatal

trait FromJavaFuture[F[_]] {
  def delayFuture[T: ClassTag](future: F[CompletableFuture[T]]): F[Option[T]]
}

object FromJavaFuture {
  def apply[F[_]](implicit fjf: FromJavaFuture[F]): FromJavaFuture[F] = fjf

  private class EffectFromJavaFuture[F[_]](implicit EF: Effect[F]) extends FromJavaFuture[F] {
    override def delayFuture[T: ClassTag](future: F[CompletableFuture[T]]): F[Option[T]] = future flatMap { f =>
      EF async { cb =>
        try {
          f whenComplete {
            case (v: T, _) =>
              cb(v.some.asRight)
            case (_, e: CompletionException) =>
              cb(e.getCause.asLeft)
            case (_, e: Throwable) =>
              cb(e.asLeft)
            case (null, null) =>
              cb(None.asRight)
          }
        } catch {
          case NonFatal(error) =>
            cb(new IllegalStateException("Passed complatable future evaluated with exception", error).asLeft)
        }
      }
    }
  }

  implicit def effectFromJava[F[_]: Effect]: FromJavaFuture[F] = new EffectFromJavaFuture[F]
}
