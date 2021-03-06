package com.azavea.rf.datamodel

import com.azavea.rf.datamodel._

import geotrellis.slick.Projected
import geotrellis.vector.{MultiPolygon, Point, Polygon}
import geotrellis.vector.testkit.Rectangle

import io.circe.Json
import io.circe.testing.ArbitraryInstances
import io.circe.syntax._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import com.lonelyplanet.akka.http.extensions.{PageRequest, Order}

import java.sql.Timestamp
import java.time.LocalDate
import java.util.UUID

object Generators extends ArbitraryInstances {

  private def stringListGen: Gen[List[String]] =
    Gen.oneOf(0, 15) flatMap { Gen.listOfN(_, nonEmptyStringGen) }

  private def nonEmptyStringGen: Gen[String] =
    Gen.nonEmptyListOf[Char](Gen.alphaChar) map { _.mkString }

  private def possiblyEmptyStringGen: Gen[String] =
    Gen.containerOf[List, Char](Gen.alphaChar) map { _.mkString }

  private def pageRequestGen: Gen[PageRequest] =
    Gen.const(PageRequest(0, 20, Map("created_at" -> Order.Desc)))

  private def userRoleGen: Gen[UserRole] = Gen.oneOf(UserRoleRole, Viewer, Admin)

  private def annotationQualityGen: Gen[AnnotationQuality] = Gen.oneOf(
    AnnotationQuality.Yes, AnnotationQuality.No, AnnotationQuality.Miss, AnnotationQuality.Unsure
  )

  private def visibilityGen: Gen[Visibility] = Gen.oneOf(
    Visibility.Public, Visibility.Organization, Visibility.Private)

  private def sceneTypeGen: Gen[SceneType] = Gen.oneOf(
    SceneType.Avro, SceneType.COG
  )

  private def credentialGen: Gen[Credential] = possiblyEmptyStringGen flatMap { Credential.fromString }

  // This is fine not to test the max value --
  private def rawDataBytesGen: Gen[Long] = Gen.choose(0L, 100000L)

  private def bandDataTypeGen: Gen[BandDataType] = Gen.oneOf(
    BandDataType.Diverging, BandDataType.Sequential, BandDataType.Categorical
  )

  private def uuidGen: Gen[UUID] = Gen.delay(UUID.randomUUID)

  private def jobStatusGen: Gen[JobStatus] = Gen.oneOf(
    JobStatus.Uploading, JobStatus.Success, JobStatus.Failure, JobStatus.PartialFailure,
    JobStatus.Queued, JobStatus.Processing
  )

  private def ingestStatusGen: Gen[IngestStatus] = Gen.oneOf(
    IngestStatus.NotIngested, IngestStatus.ToBeIngested, IngestStatus.Ingesting,
    IngestStatus.Ingested, IngestStatus.Failed
  )

  private def shapePropertiesGen: Gen[ShapeProperties] = for {
    timeField <- timestampIn2016Gen
    userField <- nonEmptyStringGen
    organizationId <- uuidGen
    name <- nonEmptyStringGen
    description <- Gen.oneOf(Gen.const(None), nonEmptyStringGen map { Some(_) })
  } yield {
    ShapeProperties(timeField, userField, timeField, userField, userField, organizationId, name, description)
  }

  private def thumbnailSizeGen: Gen[ThumbnailSize] = Gen.oneOf(
    ThumbnailSize.Small, ThumbnailSize.Large, ThumbnailSize.Square
  )

  private def uploadStatusGen: Gen[UploadStatus] = Gen.oneOf(
    UploadStatus.Created, UploadStatus.Uploading, UploadStatus.Uploaded, UploadStatus.Queued,
    UploadStatus.Processing, UploadStatus.Complete, UploadStatus.Failed, UploadStatus.Aborted
  )

  private def uploadTypeGen: Gen[UploadType] = Gen.oneOf(
    UploadType.Dropbox, UploadType.S3, UploadType.Local, UploadType.Planet
  )

  private def fileTypeGen: Gen[FileType] = Gen.oneOf(
    FileType.Geotiff, FileType.GeotiffWithMetadata
  )

  private def timestampIn2016Gen: Gen[Timestamp] = for {
    year <- Gen.const(2016)
    month <- Gen.choose(1, 12)
    day <- Gen.choose(1, 28) // for safety
  } yield { Timestamp.valueOf(LocalDate.of(year, month, day).atStartOfDay) }


  // generate up to a 50km/side polygon with bounds in EPSG:3857 bounds
  private def polygonGen3857: Gen[Polygon] = for {
    width <- Gen.choose(100, 50000)
    height <- Gen.choose(100, 50000)
    centerX <- Gen.choose(-2E7, 2E7)
    centerY <- Gen.choose(-2E7, 2E7)
  } yield {
    Rectangle().withWidth(width).withHeight(height).setCenter(Point(centerX, centerY)).build()
  }

  private def multiPolygonGen3857: Gen[MultiPolygon] = for {
    polygons <- Gen.oneOf(1, 2) flatMap { Gen.listOfN[Polygon](_, polygonGen3857) }
  } yield (MultiPolygon(polygons))

  private def projectedMultiPolygonGen3857: Gen[Projected[MultiPolygon]] =
    multiPolygonGen3857 map { Projected(_, 3857) }

  private def annotationCreateGen: Gen[Annotation.Create] = for {
    owner <- Gen.const(None)
    label <- nonEmptyStringGen
    description <- nonEmptyStringGen map { Some(_) }
    machineGenerated <- arbitrary[Option[Boolean]]
    confidence <- Gen.choose(0.0f, 1.0f) map { Some(_) }
    quality <- annotationQualityGen map { Some(_) }
    geometry <- projectedMultiPolygonGen3857 map { Some(_) }
  } yield {
    Annotation.Create(
      owner, label, description, machineGenerated, confidence, quality, geometry
    )
  }

  private def annotationGen: Gen[Annotation] = for {
    user <- userGen
    projectId <- uuidGen
    annotationCreate <- annotationCreateGen
  } yield {
    annotationCreate.toAnnotation(projectId, user)
  }

  private def organizationCreateGen: Gen[Organization.Create] = for {
    name <- nonEmptyStringGen
  } yield (Organization.Create(name))

  private def organizationGen: Gen[Organization] = organizationCreateGen map { _.toOrganization }

  private def shapeCreateGen: Gen[Shape.Create] = for {
    owner <- Gen.const(None)
    organizationId <- uuidGen
    name <- nonEmptyStringGen
    description <- nonEmptyStringGen map { Some(_) }
    geometry <- Gen.oneOf(Gen.const(None), projectedMultiPolygonGen3857 map { Some(_) })
  } yield {
    Shape.Create(owner, organizationId, name, description, geometry)
  }

  private def shapeGeoJSONGen: Gen[Shape.GeoJSON] = for {
    id <- uuidGen
    geometry <- Gen.oneOf(Gen.const(None), projectedMultiPolygonGen3857 map { Some(_) })
    properties <- shapePropertiesGen
  } yield {
    Shape.GeoJSON(id, geometry, properties)
  }

  private def userCreateGen: Gen[User.Create] = for {
    id <- nonEmptyStringGen
    org <- organizationGen
    role <- userRoleGen
  } yield { User.Create(id, org.id, role) }

  private def userGen: Gen[User] = userCreateGen map { _.toUser }

  private def bandIdentifiedGen: Gen[Band.Identified] = for {
    name <- nonEmptyStringGen
    number <- Gen.choose(1, 15)
    wavelength <- Gen.listOfN(2, Gen.choose(1, 50000)) map { _.sorted }
    imageId <- uuidGen
  } yield { Band.Identified(None, imageId, name, number, wavelength) }

  private def bandCreateGen: Gen[Band.Create] = for {
    name <- nonEmptyStringGen
    number <- Gen.choose(1, 15)
    wavelength <- Gen.listOfN(2, Gen.choose(1, 50000)) map { _.sorted }
  } yield { Band.Create(name, number, wavelength) }

  private def bandGen: Gen[Band] = bandIdentifiedGen map { _.toBand }

  private def singleBandOptionsParamsGen: Gen[SingleBandOptions.Params] = for {
    band <- Gen.choose(1, 15)
    datatype <- bandDataTypeGen
    colorBins <- Gen.choose(3, 17)
    colorScheme <- Gen.const(().asJson)
    legendOrientation <- nonEmptyStringGen
  } yield { SingleBandOptions.Params(band, datatype, colorBins, colorScheme, legendOrientation) }

  private def imageCreateGen: Gen[Image.Create] = for {
    orgId <- uuidGen
    rawDataBytes <- rawDataBytesGen
    visibility <- visibilityGen
    filename <- nonEmptyStringGen
    sourceUri <- nonEmptyStringGen
    scene <- uuidGen
    imageMetadata <- Gen.const(().asJson)
    owner <- arbitrary[Option[String]]
    resolutionMeters <- Gen.choose(0.25f, 1000f)
    metadataFiles <- stringListGen
  } yield (
    Image.Create(
      orgId, rawDataBytes, visibility, filename, sourceUri, scene, imageMetadata,
      owner, resolutionMeters, metadataFiles
    )
  )

  private def imageBandedGen: Gen[Image.Banded] = for {
    orgId <- uuidGen
    rawDataBytes <- rawDataBytesGen
    visibility <- visibilityGen
    filename <- nonEmptyStringGen
    sourceUri <- nonEmptyStringGen
    scene <- uuidGen
    imageMetadata <- Gen.const(().asJson)
    owner <- arbitrary[Option[String]]
    resolutionMeters <- Gen.choose(0.25f, 1000f)
    metadataFiles <- stringListGen
    bands <- Gen.listOfN(3, bandCreateGen)
  } yield (
    Image.Banded(
      orgId, rawDataBytes, visibility, filename, sourceUri, owner, scene, imageMetadata,
      resolutionMeters, metadataFiles, bands
    )
  )
  private def imageGen: Gen[Image] = for {
    imCreate <- imageCreateGen
    user <- userGen
  } yield (imCreate.copy(owner=Some(user.id)).toImage(user))

  private def projectCreateGen: Gen[Project.Create] = for {
    orgId <- uuidGen
    name <- nonEmptyStringGen
    description <- nonEmptyStringGen
    visibility <- visibilityGen
    tileVisibility <- visibilityGen
    isAOIProject <- arbitrary[Boolean]
    aoiCadenceMillis <- Gen.choose(0L, 604800000L)
    owner <- Gen.const(None)
    tags <- stringListGen
    isSingleBand <- arbitrary[Boolean]
    singleBandOptions <- singleBandOptionsParamsGen map { Some(_) }
  } yield {
    Project.Create(
      orgId, name, description, visibility, tileVisibility, isAOIProject, aoiCadenceMillis,
      owner, tags, isSingleBand, singleBandOptions
    )
  }

  private def projectGen: Gen[Project] = for {
    projCreate <- projectCreateGen
    user <- userGen
  } yield { projCreate.copy(owner = Some(user.id)).toProject(user) }

  private def sceneFilterFieldsGen: Gen[SceneFilterFields] = for {
    cloudCover <- Gen.frequency((1, None), (10, Gen.choose(0.0f, 1.0f) map { Some(_) }))
    acquisitionDate <- Gen.frequency((1, None), (10, timestampIn2016Gen map { Some(_) }))
    sunAzimuth <- Gen.frequency((1, None), (10, Gen.choose(0f, 360f) map { Some(_) }))
    sunElevation <- Gen.frequency((1, None), (10, Gen.choose(0f, 90f) map { Some(_) }))
  } yield { SceneFilterFields(cloudCover, acquisitionDate, sunAzimuth, sunElevation) }

  private def sceneStatusFieldsGen: Gen[SceneStatusFields] = for {
    thumbnailStatus <- jobStatusGen
    boundaryStatus <- jobStatusGen
    ingestStatus <- ingestStatusGen
  } yield { SceneStatusFields(thumbnailStatus, boundaryStatus, ingestStatus) }

  private def thumbnailIdentifiedGen: Gen[Thumbnail.Identified] = for {
    id <- uuidGen map { Some(_) }
    organizationId <- uuidGen
    thumbnailSize <- thumbnailSizeGen
    sideLength <- Gen.choose(200, 1000)
    sceneId <- uuidGen
    url <- nonEmptyStringGen
  } yield {
    Thumbnail.Identified(id, organizationId, thumbnailSize, sideLength, sideLength, sceneId, url)
  }

  private def thumbnailGen: Gen[Thumbnail] = for {
    thumbnailIdentified <- thumbnailIdentifiedGen
    userId <- nonEmptyStringGen
  } yield { thumbnailIdentified.toThumbnail(userId) }

  private def sceneCreateGen: Gen[Scene.Create] = for {
    sceneId <- uuidGen map { Some(_) }
    organizationId <- uuidGen
    ingestSizeBytes <- Gen.const(0)
    visibility <- visibilityGen
    tags <- stringListGen
    datasource <- uuidGen
    sceneMetadata <- Gen.const(().asJson)
    name <- nonEmptyStringGen
    owner <- arbitrary[Option[String]]
    tileFootprint <- projectedMultiPolygonGen3857 map { Some(_) }
    dataFootprint <- projectedMultiPolygonGen3857 map { Some(_) }
    metadataFiles <- stringListGen
    images <- Gen.oneOf(1, 10) flatMap { Gen.listOfN(_, imageBandedGen) }
    thumbnails <- Gen.oneOf(1, 2) flatMap { Gen.listOfN(_, thumbnailIdentifiedGen) }
    ingestLocation <- Gen.oneOf(nonEmptyStringGen map { Some(_) }, Gen.const(None))
    filterFields <- sceneFilterFieldsGen
    statusFields <- sceneStatusFieldsGen
    sceneType <- Gen.option(sceneTypeGen)
  } yield {
    Scene.Create(sceneId, organizationId, ingestSizeBytes, visibility, tags,
                 datasource, sceneMetadata, name, owner, tileFootprint, dataFootprint,
                 metadataFiles, images, thumbnails, ingestLocation, filterFields, statusFields,
                 sceneType)
  }

  private def aoiGen: Gen[AOI] = for {
    id <- uuidGen
    timeField <- timestampIn2016Gen
    organizationId <- uuidGen
    userField <- nonEmptyStringGen
    area <- projectedMultiPolygonGen3857
    filters <- Gen.const(().asJson) // maybe this should be CombinedSceneQueryParams as json
    isActive <- arbitrary[Boolean]
  } yield {
    AOI(id, timeField, timeField, organizationId, userField, userField, userField, area, filters, isActive)
  }

  private def datasourceCreateGen: Gen[Datasource.Create] = for {
    orgId <- uuidGen
    name <- nonEmptyStringGen
    visibility <- visibilityGen
    owner <- arbitrary[Option[String]]
    composites <- Gen.delay(().asJson)
    extras <- Gen.delay(().asJson)
    bands <- Gen.delay(().asJson)
    licenseName <- Gen.oneOf(None, Some("GPL-3.0"))
  } yield {
    Datasource.Create(orgId, name, visibility, owner, composites, extras, bands, licenseName)
  }

  private def uploadCreateGen: Gen[Upload.Create] = for {
    organizationId <- uuidGen
    uploadStatus <- uploadStatusGen
    fileType <- fileTypeGen
    uploadType <- uploadTypeGen
    files <- stringListGen
    datasource <- uuidGen
    metadata <- Gen.const(().asJson)
    owner <- Gen.const(None)
    visibility <- visibilityGen
    projectId <- Gen.const(None)
    source <- Gen.oneOf(nonEmptyStringGen map { Some(_) }, Gen.const(None))
  } yield {
    Upload.Create(organizationId, uploadStatus, fileType, uploadType, files, datasource, metadata,
                  owner, visibility, projectId, source)
  }

  private def layerAttributeGen: Gen[LayerAttribute] = for {
    layerName <- nonEmptyStringGen
    zoom <- Gen.choose(0, 30)
    name <- nonEmptyStringGen
    value <- Gen.const(().asJson)
  } yield {
    LayerAttribute(layerName, zoom, name, value)
  }

  private def layerAttributesWithSameLayerNameGen: Gen[List[LayerAttribute]] = for {
    layerName <- nonEmptyStringGen
    layerAttributes <- Gen.listOfN(10, layerAttributeGen)
  } yield layerAttributes map { _.copy(layerName = layerName) }

  private def combinedSceneQueryParamsGen: Gen[CombinedSceneQueryParams] =
    Gen.const(CombinedSceneQueryParams())

  object Implicits {
    implicit def arbCredential: Arbitrary[Credential] = Arbitrary { credentialGen }

    implicit def arbPageRequest: Arbitrary[PageRequest] = Arbitrary { pageRequestGen }

    implicit def arbCombinedSceneQueryParams: Arbitrary[CombinedSceneQueryParams] = Arbitrary { combinedSceneQueryParamsGen }

    implicit def arbAnnotationCreate: Arbitrary[Annotation.Create] = Arbitrary { annotationCreateGen }

    implicit def arbListAnnotationCreate: Arbitrary[List[Annotation.Create]] = Arbitrary {
      Gen.listOfN(10, arbitrary[Annotation.Create])
    }

    implicit def arbAnnotation: Arbitrary[Annotation] = Arbitrary { annotationGen }

    implicit def arbOrganization: Arbitrary[Organization] = Arbitrary { organizationGen }

    implicit def arbOrganizationCreate: Arbitrary[Organization.Create] = Arbitrary { organizationCreateGen }

    implicit def arbUserCreate: Arbitrary[User.Create] = Arbitrary { userCreateGen }

    implicit def arbUser: Arbitrary[User] = Arbitrary { userGen }

    implicit def arbBand: Arbitrary[Band] = Arbitrary { bandGen }

    implicit def arbImage: Arbitrary[Image] = Arbitrary { imageGen }

    implicit def arbImageCreate: Arbitrary[Image.Create] = Arbitrary { imageCreateGen }

    implicit def arbImageBanded: Arbitrary[Image.Banded] = Arbitrary { imageBandedGen }

    implicit def arbProjectCreate: Arbitrary[Project.Create] = Arbitrary { projectCreateGen }

    implicit def arbProject: Arbitrary[Project] = Arbitrary { projectGen }

    implicit def arbSceneCreate: Arbitrary[Scene.Create] = Arbitrary { sceneCreateGen }

    implicit def arbShapeCreate: Arbitrary[Shape.Create] = Arbitrary { shapeCreateGen }

    implicit def arbShapeGeoJSON: Arbitrary[Shape.GeoJSON] = Arbitrary { shapeGeoJSONGen }

    implicit def arbListSceneCreate: Arbitrary[List[Scene.Create]] = Arbitrary { Gen.listOfN(3, sceneCreateGen) }

    implicit def arbThumbnail: Arbitrary[Thumbnail] = Arbitrary { thumbnailGen }

    implicit def arbDatasourceCreate: Arbitrary[Datasource.Create] = Arbitrary { datasourceCreateGen }

    implicit def arbUploadCreate: Arbitrary[Upload.Create] = Arbitrary { uploadCreateGen }

    implicit def arbAOI: Arbitrary[AOI] = Arbitrary { aoiGen }

    implicit def arbLayerAttribute: Arbitrary[LayerAttribute] = Arbitrary { layerAttributeGen }

    implicit def arbListLayerAttribute: Arbitrary[List[LayerAttribute]] = Arbitrary {
      layerAttributesWithSameLayerNameGen
    }
  }
}
