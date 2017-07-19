package com.azavea.rf.tile

import com.azavea.rf.database.Database
import com.azavea.rf.tile.routes._
import com.azavea.rf.tile.tool._
import com.azavea.rf.common.utils._

import ch.megard.akka.http.cors.CorsDirectives._
import ch.megard.akka.http.cors.CorsSettings

import akka.http.scaladsl.server._
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.ExecutionContext

/**
  *
  * The idea:
  *  1. To use default non-blocking akka-http execution context for routes, for all requests.
  *  2. To use one really fat execution context for all blocking operations.
  *
  *  - Blocking operations are operations which can block the context due to long computation.
  *  - All bloking Futures should not be declared in the same execution context as all server requests do
  *  - All map functions over such Futures should be in their own execution context
  *
  * */
class Router extends LazyLogging
    with TileAuthentication
    with TileErrorHandler {

  implicit lazy val database = Database.DEFAULT
  implicit val system = AkkaSystem.system
  implicit val materializer = AkkaSystem.materializer
  implicit val executionContext: ExecutionContext = system.dispatcher
  implicit val blockingExecutionContext: BlockingExecutionContext = BlockingExecutionContext(system.dispatchers.lookup("blocking-io-dispatcher"))
  val heavyBlockingExecutionContext: BlockingExecutionContext = BlockingExecutionContext(system.dispatchers.lookup("blocking-io-dispatcher"))

  val toolRoutes = new ToolRoutes()

  val corsSettings = CorsSettings.defaultSettings

  def root = cors() {
    handleExceptions(tileExceptionHandler) {
      pathPrefix("tiles") {
        pathPrefix(JavaUUID) { projectId =>
          tileAccessAuthorized(projectId) {
            case true => MosaicRoutes.mosaicProject(projectId)
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
          SceneRoutes.root(executionContext, heavyBlockingExecutionContext)
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
