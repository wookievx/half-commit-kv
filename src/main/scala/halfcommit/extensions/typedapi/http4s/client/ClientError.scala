package halfcommit.extensions.typedapi.http4s.client

sealed abstract class ClientError(code: Int, message: String, cause: Option[Throwable] = None)
  extends Exception(s"Client request failed, code: $code, message: $message", cause.orNull)

case class ResponseError(code: Int, message: String) extends ClientError(code, message)
case class DecodeError(message: String, cause: Option[Throwable] = None) extends ClientError(502, message, cause)
