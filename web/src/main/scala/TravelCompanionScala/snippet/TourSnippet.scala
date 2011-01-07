package TravelCompanionScala {
package snippet {

import _root_.scala.xml.{NodeSeq, Text}

import _root_.net.liftweb._
import common.{Box}
import http._
import S._
import util._
import Helpers._

import TravelCompanionScala.model._
import api.tourVarFromAPI

/**
 * Created by IntelliJ IDEA.
 * User: Ralf Muri
 * Date: 09.04.2010
 * Time: 17:14:14
 * To change this template use File | Settings | File Templates.
 */


object TourEnum extends Enumeration {
  val OWN_TOURS = Value("OwnTours")
  val OTHERS_TOURS = Value("OthersTours")
}

// Set up a requestVar to track the TOUR object for edits and adds
object tourVar extends RequestVar[Tour](new Tour())

class TourSnippet {
  def tour = tourVar.is

  def doRemove() = {
    val t = Model.merge(tour)
    Model.remove(t)
    S.redirectTo("/tour/list")
  }

  def showTour = {
    var currentTour = tour

    //The requestVar is filled with a default tour, because Request comes from GridAPI via the sessionVar
    if (currentTour.id == 0) {
      currentTour = tourVarFromAPI.is
    }

    "#tour_name" #> currentTour.name &
    "#tour_description" #> currentTour.description &
    "#tour_edit" #> SHtml.link("edit", () => tourVar(currentTour), Text(?("tour.editTour"))) &
    "#tour_newStage" #> SHtml.link("stage/edit", () => tourVar(currentTour), Text(?("tour.newStage")))
  }

  // Utility methods for processing a submitted form
  def is_valid_Tour_?(toCheck: Tour): Boolean =
    List((if (toCheck.name.length == 0) {S.error(S.?("tour.noName")); false} else true),
      (if (toCheck.owner == null) {S.error(S.?("tour.noOwner")); false} else true)).forall(_ == true)

  def editTour = {
    def doEdit() = {
      if (is_valid_Tour_?(tour)) {
        Model.mergeAndFlush(tour)
        S.redirectTo("/tour/list")
      }
    }

    val currentTour = tour

    if (currentTour.owner == null) {
      currentTour.owner = UserManagement.currentUser
    }

    "#tour_name" #> SHtml.text(currentTour.name, currentTour.name = _) &
    "#tour_description" #> SHtml.textarea(currentTour.description, currentTour.description = _) &
    "type=submit" #> SHtml.submit(?("save"), () => {tourVar(currentTour); doEdit})
  }


  def listTours = {
    val which = S.attr("which").map(_.toString) openOr "AllTours"

    ".tour" #> tours(TourEnum.withName(which)).map(tour => {
      "#tour_name *" #> SHtml.link("view", () => tourVar(tour), Text(tour.name)) &
              "#tour_description *" #> tour.description &
              "#tour_creator *" #> tour.owner.name &
              "#tour_addStage *" #> SHtml.link("stage/edit", () => tourVar(tour), Text(?("tour.addStage"))) &
              "#tour_edit *" #> SHtml.link("edit", () => tourVar(tour), Text(?("edit"))) &
              "#tour_view *" #> SHtml.link("view", () => tourVar(tour), Text(?("view"))) &
              "#tour_remove *" #> SHtml.link("remove", () => {
                tourVar(tour); doRemove
              }, Text(?("remove")))
    })
  }

  private def tours(which: TourEnum.Value): List[Tour] = {
    which match {
      case TourEnum.OWN_TOURS => Model.createNamedQuery[Tour]("findTourByOwner").setParams("owner" -> UserManagement.currentUser).findAll.toList
      case TourEnum.OTHERS_TOURS => Model.createNamedQuery[Tour]("findTourByOthers").setParams("owner" -> UserManagement.currentUser).findAll.toList
    }
  }
}
}
}