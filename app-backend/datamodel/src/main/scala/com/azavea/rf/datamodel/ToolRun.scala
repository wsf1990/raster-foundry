package com.azavea.rf.datamodel

import java.util.UUID
import java.sql.Timestamp

import io.circe._
import io.circe.generic.JsonCodec

@JsonCodec
case class ToolRun(
  id: UUID,
  createdAt: Timestamp,
  createdBy: String,
  modifiedAt: Timestamp,
  modifiedBy: String,
  owner: String,
  visibility: Visibility,
  organizationId: UUID,
  tool: UUID,
  ast: Json,
  metadata: Json
)

object ToolRun {
  def create = Create.apply _
  def tupled = (ToolRun.apply _).tupled

  @JsonCodec
  case class Create(
    visibility: Visibility,
    organizationId: UUID,
    tool: UUID,
    ast: Json,
    owner: Option[String],
    metadata: Json
  ) extends OwnerCheck {
    def toToolRun(user: User): ToolRun = {

      val now = new Timestamp((new java.util.Date).getTime)

      val ownerId = checkOwner(user, this.owner)

      ToolRun(
        UUID.randomUUID,
        now,
        user.id,
        now,
        user.id,
        ownerId,
        visibility,
        organizationId,
        tool,
        ast,
        metadata
      )
    }
  }
}
