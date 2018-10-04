package halfcommit.model

import com.github.nscala_time.time.Imports._
import halfcommit.storage.Storage.Revision

case class Transaction(started: DateTime, revision: Revision)
