package halfcommit.typedapi.http4s.server

import typedapi.server.ValueExtractor

import scala.util.Try

trait PrimitiveExtractors extends JsonExtractor {

  implicit val intExtractor = new ValueExtractor[Int] {
    override def typeDesc: String = "Integer"
    override def apply(v1: String): Option[Int] = Try(v1.toInt).toOption
  }

  implicit val stringExtractor = new ValueExtractor[String] {
    override def typeDesc: String = "String"
    override def apply(v1: String): Option[String] = Some(v1)
  }

  implicit val booleanExtractor = new ValueExtractor[Boolean] {
    override def typeDesc: String = "Boolean"
    override def apply(v1: String): Option[Boolean] = Try(v1.toBoolean).toOption
  }

  implicit val doubleExtractor = new ValueExtractor[Double] {
    override def typeDesc: String = "Double"
    override def apply(v1: String): Option[Double] = Try(v1.toDouble).toOption
  }

}