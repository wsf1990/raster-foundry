package com.azavea.rf.tile

import ch.megard.akka.http.cors.CorsDirectives._
import ch.megard.akka.http.cors.CorsSettings
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server._
import com.azavea.rf.database.Database
import com.azavea.rf.tile.routes._
import com.azavea.rf.tile.tool._
import com.azavea.rf.tile.util.TimingLogging
import com.typesafe.scalalogging.LazyLogging

class Router extends LazyLogging
    with TileAuthentication
    with TileErrorHandler with TimingLogging {

  implicit lazy val database = Database.DEFAULT
  implicit val system = AkkaSystem.system
  implicit val materializer = AkkaSystem.materializer

  val toolRoutes = new ToolRoutes()

  val corsSettings = CorsSettings.defaultSettings

  def root = cors() {
    handleExceptions(tileExceptionHandler) {
      pathPrefix("tiles") {
        pathPrefix(JavaUUID) { projectId =>
          val result =
            timedCreate("tiles", "Router.scala::30 start", "Router.scala::30 finish") {
              tileAccessAuthorized(projectId) {
                case true => MosaicRoutes.mosaicProject(projectId)(database)
                case _ => reject(AuthorizationFailedRejection)
              }
            }

          printBuffer("tiles")
          result
        } ~
        pathPrefix("healthcheck") {
          pathEndOrSingleSlash {
            get {
              HealthCheckRoute.root
            }
          }
        } ~
        tileAuthenticateOption { _ =>
          SceneRoutes.root
        } ~
        pathPrefix("tools") {
          get {
            tileAuthenticateOption { _ =>
              toolRoutes.tms(TileSources.cachedTmsSource) ~
              toolRoutes.validate ~
              toolRoutes.preflight
            }
          }
        }
      }
    }
  }
}
