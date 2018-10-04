package halfcommit.storage.handlers

import cats.effect.Effect
import cats.syntax.all._
import com.github.nscala_time.time.Imports._
import halfcommit.model.Transaction
import halfcommit.storage.CommitableStorage.IllegalOperationError
import halfcommit.storage.Storage.{Document, Revised, Revision}
import halfcommit.storage.handlers.BasicCommitableStorage._
import halfcommit.storage.{CommitableStorage, Storage}
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import io.circe.generic.semiauto._
import shapeless.tag.@@

class BasicCommitableStorage[F[_] : Effect](implicit
  storage: Storage[F],
  collection: String @@ CollectionName
) extends CommitableStorage[F] {
  private val F = Effect[F]

  override def get(key: String): F[Option[Json]] =
    storage.getDocument[StorageObject](
      collection,
      key
    ).map(_ flatMap {
      case Revised(StorageObject(c, _, _), revision) =>
        (c deepMerge Json.obj("_rev" := revision)).some
    })

  private def createTransaction[T](
    key: String,
    document: Json,
    revision: String,
    opType: OpType
  )(
    opFunction: (String, Document[StorageObject]) => F[Revised[T]],
    createFunc: (String, Document[StorageObject]) => F[Revised[T]]
  ): F[(Transaction, T)] =
    for {
      state <- storage.getDocument[StorageObject](
        collection,
        key
      )
      res <- state match {
        case Some(Revised(so, _)) if so.transaction.isEmpty =>
          opFunction(
            collection,
            Document(
              key,
              so.copy(transaction = opType.some, transacted = document.some),
              revision.some
            )
          )
        case Some(Revised(StorageObject(_, Some(transaction), transacted), _)) =>
          F.raiseError(
            IllegalOperationError(
              412,
              s"There is ongoing transaction for the given object, key: $key, type: $transaction, expected change: $transacted"
            )
          )
        case None =>
          createFunc(
            collection,
            Document(
              key,
              StorageObject(
                document,
                Create.some,
                document.some
              ),
              revision.some
            )
          )
      }
      date <- F.delay(DateTime.now())
    } yield Transaction(date, res.revision) -> res.value


  override def put(key: String, document: Json, revision: String): F[Transaction] =
    createTransaction(key, document, revision, Put)(
      storage.putDocument(_, _),
      storage.createDocument(_, _)
    ) map { case (t, _) => t }

  override def patch(key: String, document: Json, revision: String): F[(Transaction, Json)] =
    createTransaction(key, document, revision, Patch)(
      storage.updateDocument(_, _).map(rv => Revised(rv.value.current, rv.revision)),
      storage.createDocument(_, _) as Revised(document, revision)
    )

  override def create(key: String, document: Json, revision: String): F[Transaction] =
    createTransaction(key, document, revision, Create)(
      (_, _) => F.raiseError[Revised[Json]](
        IllegalOperationError(412, "Cannot create transaction for creation, object already exists")
      ),
      storage.createDocument(_, _) as Revised(document, revision)
    ) map { case (t, _) => t }

  override def delete(key: String, revision: String): F[Transaction] =
    createTransaction(key, Json.Null, revision, Delete)(
      storage.updateDocument(_, _).map(rv => Revised(Json.Null, rv.revision)),
      (_, _) => F.raiseError[Revised[Json]](
        IllegalOperationError(412, "Cannot create delete transaction for non existing object")
      )
    ) map { case (t, _) => t }

  override def commitTransaction(key: String, revision: String): F[Option[Revision]] = for {
    state <- storage.getDocument[StorageObject](
      collection,
      key
    )
    rev <- state match {
      case Some(Revised(StorageObject(_, Some(Create | Put), Some(obj)), _)) =>
        storage.putDocument(
          collection,
          Document(
            key,
            StorageObject(
              obj,
              None,
              None
            ),
            revision.some
          )
        ).map(_.revision.some)
      case Some(Revised(StorageObject(current, Some(Patch), Some(obj)), _)) =>
        storage.updateDocument(
          collection,
          Document(
            key,
            StorageObject(
              current deepMerge obj,
              None,
              None
            ),
            revision.some
          )
        ).map(_.revision.some)
      case Some(so) if so.transaction contains Delete =>
        storage.deleteDocument(collection, key, revision.some) as Option.empty[Revision]
      case _ =>
        F.raiseError[Option[Revision]](IllegalOperationError(404, s"No transaction can be committed for object: $key"))
    }
  } yield rev
}

object BasicCommitableStorage {

  implicit def instance[F[_] : Effect](implicit
    storage: Storage[F],
    collection: String @@ CollectionName
  ): BasicCommitableStorage[F] = new BasicCommitableStorage[F]

  trait CollectionName

  sealed trait OpType

  final case object Put extends OpType

  final case object Delete extends OpType

  final case object Create extends OpType

  final case object Patch extends OpType

  implicit val decodeOpType: Decoder[OpType] = deriveDecoder[OpType]
  implicit val encodeOpType: Encoder[OpType] = deriveEncoder[OpType]

  private case class StorageObject(
    current: Json,
    transaction: Option[OpType],
    transacted: Option[Json]
  )

  private object StorageObject {
    implicit val encoder: Encoder[StorageObject] = deriveEncoder
    implicit val decoder: Decoder[StorageObject] = deriveDecoder
  }

}
