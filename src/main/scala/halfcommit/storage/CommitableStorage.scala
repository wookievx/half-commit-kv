package halfcommit.storage

import halfcommit.model.Transaction
import io.circe.Json

trait CommitableStorage[F[_]] {
  def get(key: String): F[Option[Json]]
  def put(key: String, document: Json, revision: String): F[Transaction]
  def patch(key: String, document: Json, revision: String): F[(Transaction, Json)]
  def create(key: String, document: Json, revision: String): F[Transaction]
  def delete(key: String, revision: String): F[Transaction]
  def commitTransaction(key: String, revision: String): F[Unit]
}

object CommitableStorage {

  case class IllegalOperationError(code: Int, message: String) extends Throwable(message)
}
