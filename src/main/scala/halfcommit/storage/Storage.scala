package halfcommit.storage

import cats.effect._
import cats.effect.concurrent._
import cats.instances.option._
import cats.syntax.all._
import cats.instances.either._
import com.arangodb.entity._
import com.arangodb.model._
import com.arangodb.{ArangoCollectionAsync, ArangoDBAsync, ArangoDBException, ArangoDatabaseAsync}
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig._
import halfcommit.storage.Storage.{Document, Revised}
import halfcommit.storage.util.FromJavaFuture
import io.circe._
import io.circe.jawn.parse
import io.circe.syntax._
import io.circe.generic.semiauto._

import scala.collection.JavaConverters._

trait Storage[F[_]] {

  def getDocument[T](collection: String, key: String)(implicit
    decoder: Decoder[T]
  ): F[Option[Revised[T]]]

  def deleteDocument[T](collection: String, key: String, revision: Option[String] = None): F[Unit]

  def createDocument[T](name: String, document: Document[T])(implicit
    encoder: Encoder[T],
    decoder: Decoder[T]
  ): F[Revised[T]]

  def putDocument[T](name: String, document: Document[T])(implicit
    encoder: Encoder[T],
    decoder: Decoder[T]
  ): F[Revised[T]]


  def updateDocument[T](name: String, document: Document[T])(implicit
    encoder: Encoder[T],
    decoder: Decoder[T]
  ): F[Revised[T]]

}

object Storage {

  case class Revised[T](value: T, revision: Revision)

  implicit def unpack[T](revised: Revised[T]): T = revised.value

  implicit def decoder[T](implicit decodeValue: Decoder[T]): Decoder[Revised[T]] = { hc =>
    val value = decodeValue(hc)
    val revision = hc.downField("_rev").as[Revision]
    (value, revision).mapN(Revised[T])
  }

  type Revision = String

  private val createOptions = (new DocumentCreateOptions).returnNew(true).waitForSync(true)
  private val replaceOptions = (new DocumentReplaceOptions).returnNew(true).waitForSync(true).ignoreRevs(false)
  private val updateOptions = (new DocumentUpdateOptions).returnNew(true).waitForSync(true).ignoreRevs(false).serializeNull(true)
  private val deleteOptions = (new DocumentDeleteOptions).returnOld(true)

  case class Document[T](key: String, value: T, revision: Option[String] = None)

  implicit def documentDecoder[T: Decoder]: Decoder[Document[T]] = deriveDecoder

  implicit def documentEncoder[T: Encoder]: Encoder[Document[T]] = deriveEncoder

  case class Versioned[T](doc: T, revision: String)

  case class StorageConfig(
    host: String,
    port: Int,
    user: String,
    password: String,
    database: String,
    numShards: Int,
    replication: Int,
    shardKeys: List[String],
    waitForSync: Boolean,
    timeout: Int,
    maxConnections: Int
  )

  case class StorageException(code: Int, message: String) extends Throwable(message)

  trait ResponseEntity[E] {
    def newDoc(entity: E): String
  }

  private implicit val documentCreateEntity: ResponseEntity[DocumentCreateEntity[String]] = _.getNew
  private implicit val documentUpdateEntity: ResponseEntity[DocumentUpdateEntity[String]] = _.getNew

  private implicit class ResponseEntityOps[F[_], E](private val entityF: F[E]) extends AnyVal {
    def newDocF[T: Decoder](implicit isEntity: ResponseEntity[E], F: Effect[F]): F[Revised[T]] =
      for {
        e <- entityF
        res <- parse(isEntity.newDoc(e)).flatMap(_.as[Revised[T]]).fold(
          F.raiseError[Revised[T]],
          F.pure
        )
      } yield res
  }


  class DefaultStorage[F[_] : ConcurrentEffect : FromJavaFuture](
    db: ArangoDatabaseAsync,
    config: StorageConfig
  ) extends Storage[F] {

    import config.numShards
    import config.replication
    import config.shardKeys

    private val F = ConcurrentEffect[F]
    private val FJF = FromJavaFuture[F]

    private val storage = F.toIO(MVar.empty[F, Map[String, ArangoCollectionAsync]]).unsafeRunSync()

    private def collectionF(collection: String): F[Option[ArangoCollectionAsync]] = {
      def fetchCollection = for {
        collectionsJ <- FJF.delayFuture(F.delay(db.getCollections))
        collection <- F.delay(
          collectionsJ.flatMap(_.asScala.find(_.getName == collection)) as db.collection(collection)
        )
      } yield collection

      for {
        s <- storage.take
        collectionObj <- s.get(collection) match {
          case s@Some(_) => F.pure(s)
          case None =>
            fetchCollection flatTap {
              case Some(c) => storage.put(s.updated(collection, c))
              case None => storage.put(s)
            }
        }
      } yield collectionObj

    }

    private def getOrCreateCollection(name: String): F[ArangoCollectionAsync] = for {
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
                  .shardKeys(shardKeys: _*))
            )
          ) as db.collection(name)
      }
    } yield collection

    private def parseRevised[T: Decoder](payload: String): F[Revised[T]] =
      parse(payload).flatMap(_.as[Revised[T]]).fold(
        F.raiseError,
        F.pure
      )

    def getDocument[T](collection: String, key: String)(implicit
      decoder: Decoder[T]
    ): F[Option[Revised[T]]] = for {
      collectionOpt <- collectionF(collection)
      docOpt <- collectionOpt traverse { c =>
        for {
          docRawOpt <-
            FJF.delayFuture(F.delay(c.getDocument[String](key, classOf[String])))
          docOpt <- docRawOpt traverse parseRevised[T]
        } yield docOpt
      }
    } yield docOpt.flatten

    def deleteDocument[T](collection: String, key: String, revision: Option[String] = None): F[Unit] = for {
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
      encoder: Encoder[T],
      decoder: Decoder[T]
    ): F[Revised[T]] = for {
      collection <- getOrCreateCollection(name)
      doc = document.value.asJson deepMerge
        Json.obj(("_key" := document.key) :: document.revision.toList.map("_rev" := _): _*)
      res <- FJF.delayFlat(
        F.delay(collection.insertDocument(doc.noSpaces, createOptions)),
        dbError
      ).newDocF[T]
    } yield res

    private def dbError = {
      StorageException(500, "DB return entity null, should not happen")
    }

    def putDocument[T](name: String, document: Document[T])(implicit
      encoder: Encoder[T],
      decoder: Decoder[T]
    ): F[Revised[T]] = for {
      collection <- getOrCreateCollection(name)
      doc = document.value.asJson deepMerge
        Json.obj(("_key" := document.key) :: document.revision.toList.map("_rev" := _): _*)
      res <- FJF.delayFlat(
          F.delay(collection.replaceDocument(document.key, doc.noSpaces, replaceOptions)),
          dbError
        ).newDocF[T]
        .recoverWith({
          case t: ArangoDBException if t.getResponseCode == 404 =>
            FJF.delayFlat(
              F.delay(collection.insertDocument(doc.noSpaces, createOptions)),
              dbError
            ).newDocF[T]
        })
    } yield res

    def updateDocument[T](name: String, document: Document[T])(implicit
      encoder: Encoder[T],
      decoder: Decoder[T]
    ): F[Revised[T]] = {

      for {
        collection <- getOrCreateCollection(name)
        doc = document.value.asJson deepMerge
          Json.obj(("_key" := document.key) :: document.revision.toList.map("_rev" := _): _*)
        updatedOpt <- FJF.delayFuture(F.delay(collection.updateDocument(document.key, doc.noSpaces, updateOptions)))
          .flatMap(_ traverse { e => parseRevised[T](e.getNew) })
          .recover({ case t: ArangoDBException if t.getResponseCode == 404 => None })
        updated <- updatedOpt match {
          case Some(d) =>
            F.pure(d)
          case None =>
            FJF.delayFlat(
              F.delay(collection.insertDocument(doc.noSpaces, createOptions)),
              dbError
            ).newDocF[T]
        }
      } yield updated
    }

  }

  def default[F[_]](implicit F: ConcurrentEffect[F]): F[Storage[F]] = {

    def arangoConnection(collectionConfig: StorageConfig): ArangoDBAsync = {
      import collectionConfig._
      (new ArangoDBAsync.Builder)
        .host(host, port)
        .user(user)
        .password(password)
        .build()
    }

    val configF: F[StorageConfig] = F catchNonFatal {
      val underlying: Config = ConfigFactory.load()
      implicit def hint[T]: ProductHint[T] = ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))
      val _ = hint
      loadConfig[StorageConfig](underlying.atPath("arangodb")).fold(
        fs => throw new Exception(fs.toList.mkString("[", ", ", "]")),
        identity
      )
    }

    for {
      config <- configF
      arango = arangoConnection(config)
      database <- F.delay(arango.db(config.database))
    } yield new DefaultStorage(database, config)
  }

}
