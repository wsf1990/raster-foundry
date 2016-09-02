package com.azavea.rf

import org.scalatest._

import com.azavea.rf.utils._

// http://www.superloopy.io/articles/2013/scala-slick-postgresql-unit-tests.html
trait DBSpec extends Suite with BeforeAndAfterAll with Config {
  // This is here just so that we can initialize the template database
  InitializeDB

  private val dbname = getClass.getSimpleName.toLowerCase
  private val driver = "org.postgresql.Driver"
  protected var db: Database = null

  override def beforeAll() {
    super.beforeAll()
    PGUtils.dropDBIfExists(jdbcNoDBUrl, dbname, dbUser, dbPassword)
    PGUtils.createDB(jdbcNoDBUrl, dbname, dbUser, dbPassword)
    db = new Database(jdbcNoDBUrl + dbname, dbUser, dbPassword)
  }

  override def afterAll() {
    super.afterAll()
    println("in after all")
    PGUtils.dropDBIfExists(jdbcNoDBUrl, dbname, dbUser, dbPassword)
  }
}

/**
 * This object is referenced at the head of [[DBSpec]] - its purpose is to carry out
 *  any one-time-only setup required for DBSpec tests. So far, that includes:
 * 1. dropping the 'template' database if one already exists (the DB from which test DBs will be copied)
 * 2. creating a new one
 * 3. running any migrations
 */
object InitializeDB extends Config {
  // delete if exists
  PGUtils.dropDBIfExists(jdbcNoDBUrl, "testing_template", dbUser, dbPassword)
  // create
  PGUtils.createDB(jdbcNoDBUrl, "testing_template", dbUser, dbPassword)
  // run migrations
  // ???
}
