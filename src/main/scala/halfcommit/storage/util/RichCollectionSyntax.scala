package halfcommit.storage.util

import com.arangodb.{ArangoCollectionAsync, ArangoDBException, ArangoDatabaseAsync}
import cats.effect._
import cats.instances.option._
import cats.syntax.all._
import com.arangodb.entity.DocumentUpdateEntity
import com.arangodb.model._
import io.circe.syntax._
import io.circe.jawn.parse
import io.circe._

import scala.collection.JavaConverters._

trait RichCollectionSyntax[F[_]] {

}

object RichCollectionSyntax {

  private val createOptions  = (new DocumentCreateOptions).returnNew(true).waitForSync(true)
  private val replaceOptions = (new DocumentReplaceOptions).returnNew(true).waitForSync(true).ignoreRevs(false)
  private val updateOptions  = (new DocumentUpdateOptions).returnNew(true).waitForSync(true).ignoreRevs(false).serializeNull(true)
  private val deleteOptions = (new DocumentDeleteOptions).returnOld(true)

  case class Document[T](key: String, value: T, revision: Option[String] = None)

  case class CollectionConfig(numShards: Int, replication: Int, shardKeys: List[String])

  val collectionConfig: CollectionConfig = CollectionConfig(
    2,
    2,
    List.empty
  )

  import collectionConfig._

  implicit class RichCollectionOps[F[_]](private val db: ArangoDatabaseAsync) extends AnyVal {

    private def collectionF(collection: String)(implicit
      F: Effect[F],
      FJF: FromJavaFuture[F],
    ): F[Option[ArangoCollectionAsync]] =
      for {
        collectionsJ <- FJF.delayFuture(F.delay(db.getCollections))
        collection <- F.delay(
          collectionsJ.flatMap(_.asScala.find(_.getName == collection)) as db.collection(collection)
        )
      } yield collection

    private def getOrCreateCollection(name: String)(implicit
      F: Effect[F],
      FJF: FromJavaFuture[F],
    ): F[ArangoCollectionAsync] = for {
      colOpt <- collectionF(name)
      collection <- colOpt match {
        case Some(c) =>
          F.pure(c)
        case None =>
          FJF.delayFuture(
            F.delay(
              db.createCollection(
                name,
                (new CollectionCreateOptions)
                  .numberOfShards(numShards)
                  .replicationFactor(replication)
                  .shardKeys(shardKeys:_*))
            )
          ) as db.collection(name)
      }
    } yield collection

    def getDocument[T](collection: String, key: String)(implicit
      F: Effect[F],
      FJF: FromJavaFuture[F],
      decoder: Decoder[T]
    ): F[Option[T]] = for {
      collectionOpt <- collectionF(collection)
      docOpt <- collectionOpt traverse { c =>
        for {
          docRawOpt <-
            FJF.delayFuture(F.delay(c.getDocument[String](key, classOf[String])))
          docOpt <- docRawOpt traverse { s =>
            parse(s).flatMap(_.as[T]).fold(
              F.raiseError[T](_),
              F.pure
            )
          }
        } yield docOpt
      }
    } yield docOpt.flatten

    def deleteDocument[T](collection: String, key: String, revision: Option[String] = None)(implicit
      F: Effect[F],
      FJF: FromJavaFuture[F]
    ): F[Unit] = for {
      collectionOpt <- collectionF(collection)
      opts = revision match {
        case Some(v) =>
          deleteOptions.ifMatch(v)
        case None =>
          deleteOptions
      }
      _ <- collectionOpt traverse { c =>
        FJF.delayFuture(F.delay(c.deleteDocument[String](key, classOf[String], opts))).void
      }
    } yield ()

    def createDocument[T](name: String, document: Document[T])(implicit
      F: Effect[F],
      FJF: FromJavaFuture[F],
      encoder: Encoder[T]
    ): F[Unit] = for {
      collection <- getOrCreateCollection(name)
      doc = document.value.asJson deepMerge
        Json.obj(("_key" := document.key) :: document.revision.toList.map("_rev" := _) :_*)
      _ <- FJF.delayFuture(F.delay(collection.insertDocument(doc.noSpaces, createOptions)))
    } yield ()

    def putDocument[T](name: String, document: Document[T])(implicit
      F: Effect[F],
      FJF: FromJavaFuture[F],
      encoder: Encoder[T]
    ): F[Unit] = for {
      collection <- getOrCreateCollection(name)
      doc = document.value.asJson deepMerge
        Json.obj(("_key" := document.key) :: document.revision.toList.map("_rev" := _) :_*)
      _ <- FJF.delayFuture(F.delay(collection.replaceDocument(document.key, doc.noSpaces, replaceOptions))).void
          .recoverWith({
            case t: ArangoDBException if t.getResponseCode == 404 =>
              FJF.delayFuture(F.delay(collection.insertDocument(document.key, doc.noSpaces, createOptions))).void
          })
    } yield ()

    def updateDocument[T](name: String, document: Document[T])(implicit
      F: Effect[F],
      FJF: FromJavaFuture[F],
      encoder: Encoder[T],
      decoder: Decoder[T]
    ): F[T] = {
      def handleEntity(updateEntity: DocumentUpdateEntity[String]): F[Option[T]] = for {
        ns <- updateEntity.getNew match {
          case s: String => F.pure(s.some)
          case _ => F.pure(None)
        }
        docOpt <- ns.traverse(parse(_).flatMap(_.as[T]).fold(
          F.raiseError[T],
          F.pure
        ))
      } yield docOpt


      for {
        collection <- getOrCreateCollection(name)
        doc = document.value.asJson deepMerge
          Json.obj(("_key" := document.key) :: document.revision.toList.map("_rev" := _) :_*)
        updatedOpt <- FJF.delayFuture(F.delay(collection.updateDocument(document.key, doc.noSpaces, updateOptions)))
          .flatMap(_ traverse handleEntity)
          .recover({ case t: ArangoDBException if t.getResponseCode == 404 => None })
        updated <- updatedOpt.flatten match {
          case Some(d) =>
            F.pure(d)
          case None =>
            FJF.delayFuture(F.delay(collection.insertDocument(document.key, doc.noSpaces, createOptions))) as document.value
        }
      } yield updated
    }

  }
}