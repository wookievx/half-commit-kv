package halfcommit.api

sealed trait ETagQuery
case class ETagValues(head: EntityTag, tail: Set[EntityTag]) extends ETagQuery
case object AnyTag extends ETagQuery


