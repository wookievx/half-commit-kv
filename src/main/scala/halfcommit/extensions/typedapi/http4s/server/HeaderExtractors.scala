package halfcommit.extensions.typedapi.http4s.server

import halfcommit.api.{ETag => _, _}
import org.http4s.headers._
import typedapi.server.ValueExtractor

trait HeaderExtractors {

  implicit val eTagExtractor: ValueExtractor[EntityTag] = new ValueExtractor[EntityTag] {
    override def typeDesc: String = "EntityTag"
    override def apply(v1: String): Option[EntityTag] = ETag.parse(v1).map(_.tag).toOption
  }

  implicit val taQueryExtractor: ValueExtractor[ETagQuery] = new ValueExtractor[ETagQuery] {
    override def typeDesc: String = "ETagQuery"

    override def apply(v1: String): Option[ETagQuery] =
      for {
        ifnm <- `If-None-Match`.parse(v1).toOption
        query <- ifnm.tags.map(l => ETagValues(l.head, l.tail.toSet))
          .orElse(if (v1.matches("""\s*\s""")) Some(AnyTag) else None)
      } yield query
  }

}
