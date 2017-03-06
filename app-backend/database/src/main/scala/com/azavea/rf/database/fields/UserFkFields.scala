package com.azavea.rf.database.fields

import com.azavea.rf.database.ExtendedPostgresDriver.api._
import com.azavea.rf.database.query.UserQueryParameters
import com.azavea.rf.database.tables.Users
import com.azavea.rf.datamodel.{User, Visibility}
import slick.lifted.ForeignKeyQuery
import java.util.UUID

trait UserFkFields { self: Table[_] =>
  def createdBy: Rep[UUID]
  def modifiedBy: Rep[UUID]

  def createdByUserFK: ForeignKeyQuery[Users, User]
  def modifiedByUserFK: ForeignKeyQuery[Users, User]
}

object UserFkFields {
  implicit class DefaultQuery[M <: UserFkFields, U, C[_]](that: Query[M, U, Seq]) {
    def filterByUser(userParams: UserQueryParameters) = {
      that.filter{ rec =>
        List(
          userParams.createdBy.map(rec.createdBy === _),
          userParams.modifiedBy.map(rec.modifiedBy === _)
        )
          .flatten
          .reduceLeftOption(_ && _)
          .getOrElse(true: Rep[Boolean])
      }
    }

    def filterToOwner(user: User) = {
      that.filter(_.createdBy === user.id)
    }
  }
}
