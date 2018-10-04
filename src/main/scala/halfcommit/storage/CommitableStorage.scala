package halfcommit.storage

import halfcommit.model.Transaction
import halfcommit.storage.Storage.Revision
import io.circe.Json

trait CommitableStorage[F[_]] {
  def get(key: String): F[Option[Json]]
  def put(key: String, document: Json, revision: Revision): F[Transaction]
  def patch(key: String, document: Json, revision: Revision): F[(Transaction, Json)]
  def create(key: String, document: Json, revision: Revision): F[Transaction]
  def delete(key: String, revision: Revision): F[Transaction]
  def commitTransaction(key: String, revision: String): F[Option[Revision]]
}

object CommitableStorage {

  case class IllegalOperationError(code: Int, message: String) extends Throwable(message)
}
