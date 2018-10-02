package halfcommit.extensions.typedapi.http4s.server

import io.circe.Decoder
import typedapi.server.ValueExtractor

import scala.reflect.ClassTag

trait JsonExtractor {

  implicit def jsonExtractor[V: Decoder](implicit
    ct: ClassTag[V]
  ): ValueExtractor[V] = new ValueExtractor[V] {
    override def typeDesc: String = ct.runtimeClass.getSimpleName

    override def apply(v1: String): Option[V] = {
      import io.circe.jawn.parse

      val eitherExtract = for {
        js <- parse(v1)
        v <- js.as[V]
      } yield v

      eitherExtract.toOption
    }
  }

}