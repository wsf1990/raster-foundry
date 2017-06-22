package com.azavea.rf.tile

import ch.megard.akka.http.cors.CorsDirectives._
import ch.megard.akka.http.cors.CorsSettings
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server._
import com.azavea.rf.database.Database
import com.azavea.rf.tile.routes._
import com.azavea.rf.tile.tool._
import com.typesafe.scalalogging.LazyLogging

class Router extends LazyLogging
    with TileAuthentication
    with TileErrorHandler with util.TimingLogging {

  implicit lazy val database = Database.DEFAULT
  implicit val system = AkkaSystem.system
  implicit val materializer = AkkaSystem.materializer

  val toolRoutes = new ToolRoutes()

  val corsSettings = CorsSettings.defaultSettings

  val rejectionHandler = RejectionHandler.default
  def logDuration(inner: Route): Route = { ctx =>
    val start = System.currentTimeMillis()
    // handling rejections here so that we get proper status codes
    val innerRejectionsHandled = handleRejections(rejectionHandler)(inner)
    mapResponse { resp =>
      val d = System.currentTimeMillis() - start
      print(s"[${resp.status.intValue()}] ${ctx.request.method.name} ${ctx.request.uri} took: ${d}ms")
      resp
    }(innerRejectionsHandled)(ctx)
  }

  def root = cors() {
    handleExceptions(tileExceptionHandler) {
      pathPrefix("tiles") {
        pathPrefix(JavaUUID) { projectId =>
          tileAccessAuthorized(projectId) {
            case true => logDuration(MosaicRoutes.mosaicProject(projectId)(database))
            case _ => reject(AuthorizationFailedRejection)
          }
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
              toolRoutes.histogram ~
              toolRoutes.preflight
            }
          }
        }
      }
    }
  }
}
