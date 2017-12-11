package com.azavea.rf.datamodel.stac

import java.util.UUID

import io.circe._
import io.circe.generic.JsonCodec

@JsonCodec
case class Asset (
  href: String,
  name: String,
  product: String,
  fileFormat: String
) {
  def toNormalized(): Asset.Normalized =
    Asset.Normalized(href, name, UUID.fromString(product), fileFormat)
}

object Asset {
  @JsonCodec
  case class Normalized(
    href: String,
    name: String,
    product: UUID,
    fileFormat: String
  ) {
    def toReferenced(): Asset =
      Asset(href, name, product.toString, fileFormat)
  }
}
