package halfcommit

import typedapi.dsl._

package object api {

  type EntityTag = org.http4s.headers.ETag.EntityTag

  val `If-Match` = Header[ETagQuery]("If-Match")
  val ETag = Header[EntityTag]("ETag")

  def endpoint[Body, ID] =
    getEndpoint[Body, ID] :|:
    putEndpoint[Body, ID] :|:
    postEndpoint[Body, ID] :|:
    deleteEndpoint[Body, ID]

  def getEndpoint[Body, ID] = := :> Segment[ID]("id") :> Get[Json, Body]
  def getDebug[Body, ID] = := :> Segment[ID]("id") :> `If-Match` :> Get[Json, Body]
  def putEndpoint[Body, ID] = := :> Segment[ID]("id") :> `If-Match` :> ReqBody[Json, Body] :> Put[Json, Body]

  def putRaw[Body, ID] = := :> Segment[ID]("id") :> ReqBody[Json, Body] :> Put[Json, Body]
  def postEndpoint[Body, ID] = := :> Segment[ID]("id") :> ETag :> ReqBody[Json, Body] :> Post[Json, Body]
  def deleteEndpoint[Body, ID] = := :> Segment[ID]("id") :> `If-Match` :> Delete[Json, Body]

}
