package com.azavea.rf.api

import com.azavea.rf.api.utils.Config
import com.azavea.rf.common.RFRejectionHandler._
import com.azavea.rf.database.Database

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer

import co.adhoclabs.akka.http.contrib.throttle.{MetricThrottleSettings, ThrottleDirective}

import scala.concurrent.ExecutionContext.Implicits.global

object AkkaSystem {
  implicit val system = ActorSystem("rf-system")
  implicit val materializer = ActorMaterializer()

  trait LoggerExecutor {
    protected implicit val log = Logging(system, "app")
  }
}

object Main extends App
    with Config
    with Router
    with AkkaSystem.LoggerExecutor
    with ThrottleDirective {
  implicit lazy val database = Database.DEFAULT
  implicit val system = AkkaSystem.system
  implicit val materializer = AkkaSystem.materializer
  implicit val throttleSettings = MetricThrottleSettings.fromConfig

  Http().bindAndHandle(throttle.apply(routes), httpHost, httpPort)

}
