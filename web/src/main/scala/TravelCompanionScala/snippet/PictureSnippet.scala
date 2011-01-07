package TravelCompanionScala.snippet

import _root_.scala.xml.{NodeSeq, Text}

import _root_.net.liftweb._
import common._
import http._
import S._
import util._
import Helpers._
import net.liftweb.imaging._

import TravelCompanionScala.model._
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, ByteArrayInputStream, InputStream}
import javax.imageio.ImageIO
import scala.collection.JavaConversions._

/**
 * Created by IntelliJ IDEA.
 * User: Ralf Muri
 * Date: 26.04.2010
 * Time: 11:06:04
 * To change this template use File | Settings | File Templates.
 */

object pictureVar extends RequestVar[Picture](new Picture)

class PictureSnippet {
  def doRemove(picture: Picture) {
    val p = Model.merge(picture)
    Model.remove(p)
    S.redirectTo("/picture/list")
  }

  var fileHolder: Box[FileParamHolder] = Empty

  def addPicture = {
    val currentPicture = new Picture

    def createThumbnail(data: Array[Byte]): Array[Byte] = {
      val in: InputStream = new ByteArrayInputStream(data)
      val original: BufferedImage = ImageIO.read(in)
      val thumbnail: BufferedImage = ImageResizer.square(None, original, 125)
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      ImageIO.write(thumbnail, "jpg", baos)
      baos.toByteArray
    }

    def doSave(picture: Picture) {
      picture.owner = UserManagement.currentUser
      fileHolder match {
        case Full(FileParamHolder(_, mime: String, _, data))
          if (mime.startsWith("image/")) => {
          picture.thumbnail = createThumbnail(data)
          picture.image = data
          picture.imageType = mime
        }
        case Full(_) => {
          S.error("Invalid Attachment")
          S.redirectTo("/picture/create")
        }
        case Empty => {
          S.error("Invalid Attachment")
          S.redirectTo("/picture/create")
        }
        case Failure(_, _, _) => {
          S.error("Invalid Attachment")
          S.redirectTo("/picture/create")
        }
      }
      if (validator.is_valid_entity_?(picture)) {
        Model.mergeAndFlush(picture)
        S.redirectTo("/picture/list")
      }
    }

    val tours = Model.createNamedQuery[Tour]("findTourByOwner").setParams("owner" -> UserManagement.currentUser).findAll.toList
    val tchoices = List("" -> ("- " + S.?("none") + " -")) ::: tours.map(tour => (tour.id.toString -> tour.name)).toList

    val entries = Model.createNamedQuery[BlogEntry]("findEntriesByOwner").setParams("owner" -> UserManagement.currentUser).findAll.toList
    val echoices = List("" -> ("- " + S.?("none") + " -")) ::: entries.map(entry => (entry.id.toString -> entry.title)).toList


    "#picture_name" #> SHtml.text(currentPicture.name, currentPicture.name = _) &
            "#picture_file" #> SHtml.fileUpload(fh => fileHolder = Full(fh)) &
            "#picture_description" #> SHtml.textarea(currentPicture.description, currentPicture.description = _) &
            "#picture_tour" #> SHtml.select(tchoices, Empty, (tourId: String) => {
              if (tourId != "") currentPicture.tour = Model.getReference(classOf[Tour], tourId.toLong) else currentPicture.tour = null
            }) &
            "#picture_blogEntry" #> SHtml.select(echoices, Empty, (entryId: String) => {
              if (entryId != "") currentPicture.blogEntry = Model.getReference(classOf[BlogEntry], entryId.toLong) else currentPicture.blogEntry = null
            }) &
            "type=submit" #> SHtml.submit(?("save"), () => doSave(currentPicture))
  }

  def showPicturesFromTour = {
    val currentTour = tourVar.is
    val pictures = Model.createNamedQuery[Picture]("findPicturesByTour").setParams("tour" -> currentTour).findAll.toList

    ".list" #> pictures.map(picture => {
      "#picture_thumbnail *" #> SHtml.link("/picture/view", () => pictureVar(picture), <img src={"/image/thumbnail/" + picture.id}/>) &
              "#picture_description *" #> picture.description
    })
  }

  def showPicture = {
    val picture = pictureVar.is

    "#picture_name" #> picture.name &
            "#picture_description" #> picture.description &
            "#picture_owner" #> picture.owner.name &
            "#picture_image" #> <img src={"/image/full/" + picture.id}/>
  }

  def listPictures(pictures: List[Picture]) = {

    def travelLinkElem(picture: Picture) = {
      if (picture.tour != null) {
        Text(S.?("tour")) ++ <br/> ++ SHtml.link("/tour/view", () => tourVar(picture.tour), Text(picture.tour.name))
      }
      else {
        NodeSeq.Empty
      }
    }

    def blogLinkElem(picture: Picture) = {
      if (picture.blogEntry != null) {
        Text(S.?("blog.entry")) ++ <br/> ++ SHtml.link("/blog/view", () => blogEntryVar(picture.blogEntry), Text(picture.blogEntry.title))
      } else {
        NodeSeq.Empty
      }
    }

    ".list" #> pictures.map(picture => {
      "#picture_thumbnail *" #> SHtml.link("/picture/view", () => pictureVar(picture), <img src={"/image/thumbnail/" + picture.id}/>) &
              "#picture_description *" #> Text(picture.description) &
              "#picture_owner *" #> Text(picture.owner.name) &
              "#picture_travel_link *" #> travelLinkElem(picture) &
              "#picture_blog_link *" #> blogLinkElem(picture) &
              "#picture_remove *" #> SHtml.link("remove", () => doRemove(picture), Text(?("remove")))
    })
  }

  def listOtherPictures = {
    val pictures = Model.createNamedQuery[Picture]("findPicturesByOthers").setParams("owner" -> UserManagement.currentUser).findAll.toList
    listPictures(pictures)
  }

  def listOwnPictures = {
    val pictures = Model.createNamedQuery[Picture]("findPicturesByOwner").setParams("owner" -> UserManagement.currentUser).findAll.toList
    listPictures(pictures)
  }
}