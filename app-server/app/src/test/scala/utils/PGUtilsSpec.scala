package com.azavea.rf.utils

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import scala.concurrent.Await
import scala.concurrent.duration._
import org.postgresql.util.PSQLException
import scala.util.Try

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}

import com.azavea.rf.utils._
import com.azavea.rf._

/**
 * This set of tests ensures the basic functionality of the utilities
 *  within PGUtils and which are depended upon for all database-reliant tests
 */
class PGUtilsSpec extends WordSpec
                     with Matchers
                     with ScalatestRouteTest
                     with Config
                     with Router
                     with DBSpec {

  implicit val ec = system.dispatcher
  implicit val database = db
  val newdb = "test1"

  "Database utilities" should {
    "Freely create and drop databases" in {
      // create
      PGUtils.createDB(jdbcNoDBUrl, newdb, dbUser, dbPassword)
      // try to create again
      val caught = intercept[org.postgresql.util.PSQLException] {
        PGUtils.createDB(jdbcNoDBUrl, newdb, dbUser, dbPassword)
      }
      // drop that db
      PGUtils.dropDB(jdbcNoDBUrl, newdb, dbUser, dbPassword)

      // create again to show that drop worked
      PGUtils.createDB(jdbcNoDBUrl, newdb, dbUser, dbPassword)

      // drop to clean house
      PGUtils.dropDB(jdbcNoDBUrl, newdb, dbUser, dbPassword)

      // Check error message
      caught.getMessage shouldBe (s"""ERROR: database "${newdb}" already exists""")
    }
  }
}

// This is here just to verify the initialization being done in DBSpec
class PGUtilsRedundantSpec extends WordSpec
                     with Matchers
                     with ScalatestRouteTest
                     with Config
                     with Router
                     with DBSpec {

  implicit val ec = system.dispatcher
  implicit val database = db
  val newdb = "test2"

  "Database utilities" should {
    "Copy databases" in {
      // create
      PGUtils.createDB(jdbcNoDBUrl, newdb, dbUser, dbPassword)
      // copy
      PGUtils.copyDB(jdbcNoDBUrl, newdb, "copyDB", dbUser, dbPassword)

      // attempt to create in the copied namespace
      val caught = intercept[org.postgresql.util.PSQLException] {
        PGUtils.createDB(jdbcNoDBUrl, "copyDB", dbUser, dbPassword)
      }

      // clean house
      PGUtils.dropDB(jdbcNoDBUrl, "copyDB", dbUser, dbPassword)
      PGUtils.dropDB(jdbcNoDBUrl, newdb, dbUser, dbPassword)

      // Check error message
      caught.getMessage shouldBe (s"""ERROR: database "copydb" already exists""")
    }
  }
}

