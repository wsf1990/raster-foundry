package com.azavea.rf

import java.text.SimpleDateFormat

import spray.json._
import spray.json.DefaultJsonProtocol._
import com.github.blemale.scaffeine.{Cache => ScaffeineCache}
import java.util.{Date, UUID}

package object tile {
  val sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")

  implicit object UUIDJsonFormat extends RootJsonFormat[UUID] {
    def write(uuid: UUID): JsValue = JsString(uuid.toString)
    def read(js: JsValue): UUID = js match {
      case JsString(uuid) => UUID.fromString(uuid)
      case _ =>
        deserializationError("Failed to parse UUID string ${js} to java UUID")
    }
  }

  implicit class withLayerCacheMethods[K, V](cache: ScaffeineCache[K, V]) extends Config {
    def take(key: K, mappingFunction: K => V): V =
      if (withCaching) cache.get(key, mappingFunction)
      else mappingFunction(key)
  }

  def printCurrentTime(id: Int = 1) = {
    val date = new Date()
    println(s"$id: ${sdf.format(date)}")
  }
}