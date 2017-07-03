package com.azavea.rf

import spray.json._
import spray.json.DefaultJsonProtocol._
import com.github.blemale.scaffeine.{Cache => ScaffeineCache}
import java.util.UUID

import scala.concurrent.{ExecutionContext, Future}

package object tile {
  implicit object UUIDJsonFormat extends RootJsonFormat[UUID] {
    def write(uuid: UUID): JsValue = JsString(uuid.toString)
    def read(js: JsValue): UUID = js match {
      case JsString(uuid) => UUID.fromString(uuid)
      case _ =>
        deserializationError("Failed to parse UUID string ${js} to java UUID")
    }
  }

  implicit class withLayerCacheMethods[K, V](cache: ScaffeineCache[K, V]) extends Config {
    def take(key: K, mappingFunction: K => V)(ec: ExecutionContext): V = {
      if (withCaching) {
        cache.getIfPresent(key) match {
          case Some(v) => v
          case _ => {
            Future { cache.get(key, mappingFunction) }
            mappingFunction(key)
          }
        }
      }
      else mappingFunction(key)
    }
  }
}