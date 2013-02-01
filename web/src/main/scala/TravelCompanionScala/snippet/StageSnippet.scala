package TravelCompanionScala.snippet

import scala.xml.{NodeSeq, Text}

import net.liftweb._
import http._

import js.JsCmds._
import js.JsCmds.JsCrVar
import js.{JsObj, JE, JsCmd}
import S._
import util._
import Helpers._
import JE._
import TravelCompanionScala.model._
import java.text.SimpleDateFormat
import net.liftmodules.widgets.autocomplete.AutoComplete
import TravelCompanionScala.api.tourVarFromAPI

/**
 * Created by IntelliJ IDEA.
 * User: Ralf Muri
 * Date: 19.04.2010
 * Time: 08:55:22
 * To change this template use File | Settings | File Templates.
 */

// Set up a requestVar to track the STAGE object for edits and adds
object stageVar extends RequestVar[Stage](new Stage())

class StageSnippet {
  def stage = stageVar.is

  def editStage  = {
    val currentTour = tourVar.is
    val currentStage = stage
    stage.tour = tourVar.is

    def doEdit() {
      if (validator.is_valid_entity_?(stage)) {
        Model.mergeAndFlush(stage)
        val currentTour = tourVar.is
        S.redirectTo("/tour/view", () => tourVar(currentTour))
      }
    }

    def setLocation(name: String, s: Stage) {
      val geos: List[String] = name.split(",").toList.map(str => str.trim)
      var loc = GeoCoder.getCurrentLocations.find(
        loc => (geos.contains(loc.name) && geos.contains(loc.countryname))
      ).getOrElse(s.destination)

      loc = Model.createQuery[Location]("SELECT l from Location l where l.geonameid = :geonameid").setParams("geonameid" -> loc.geonameid).findOne.getOrElse(loc)

      s.destination = loc
    }

    if (currentStage.destination == null) {
      currentStage.destination = new Location
    }
    if (currentStage.startdate == null) {
      currentStage.startdate = TimeHelpers.now
    }


    "#stage_title" #> SHtml.text(currentStage.name, currentStage.name = _) &
    "#stage_destination" #> AutoComplete(currentStage.destination.name, (current, limit) => {
        GeoCoder.findLocationsByName(current).map(loc => loc.name + ", " + loc.countryname)
      }, s => setLocation(s, currentStage)) &
    "#stage_description" #> SHtml.textarea(currentStage.description, currentStage.description = _) &
    "#stage_startdate" #> SHtml.text(Util.slashDate.format(currentStage.startdate), (p: String) => currentStage.startdate = Util.slashDate.parse(p)) &
    "type=submit" #> SHtml.submit(S.?("save"), () => {
        stageVar(currentStage); tourVar(currentTour); doEdit()
      })
   }


  def viewStage = {
    stage.tour = tourVar.is

    "#stage_title" #> Text(stage.name) &
    "#stage_date" #> Text(Util.slashDate.format(stage.startdate)) &
    "#stage_destination" #> Text(stage.destination.name + ", " + stage.destination.countryname)
  }

  def cvt(stage: Stage): JsObj = {
    JsObj(("title", stage.destination.name),
      ("lat", stage.destination.lat),
      ("lng", stage.destination.lng))
  }

  def ajaxFunc(stages: List[Stage]): JsCmd = {
    val locobj = stages.map(stage => cvt(stage))

    JsCrVar("locations", JsObj(("stages", JsArray(locobj: _*)))) & JsRaw("generate(locations)").cmd

  }

  def renderGoogleMap(xhtml: NodeSeq): NodeSeq = {
    val currentTour = tourVar.is
    val maptype = S.attr("type").map(_.toString) openOr "SINGLE"
    var stages: List[Stage] = List()

    if (maptype.equals("ALL")) {
      stages = Model.createNamedQuery[Stage]("findStagesByTour").setParams("tour" -> currentTour).findAll.toList
    } else {
      stages = List(stage)
    }

    (<head>
      {Script(OnLoad(ajaxFunc(stages)))}
    </head>)
  }


  def doRemove() {
    val s = Model.merge(stage)
    Model.remove(s)
    val currentTour = tourVar.is
    S.redirectTo("/tour/view", () => tourVar(currentTour))
  }

  def showStagesFromTour = {
    var currentTour = tourVar.is

    if (currentTour.id == 0) {
      currentTour = tourVarFromAPI.is
    }

    val stages = Model.createNamedQuery[Stage]("findStagesByTour").setParams("tour" -> currentTour).findAll.toList

    ".stage" #> stages.map(stage => {
      "#startdate *" #> new SimpleDateFormat("dd.MM.yyyy").format(stage.startdate) &
              "#title *" #> SHtml.link("/tour/stage/view", () => stageVar(stage), Text(stage.name)) &
              "#destination *" #> stage.destination.name &
              "#description *" #> stage.description &
              "#edit *" #> SHtml.link("/tour/stage/edit", () => {
                stageVar(stage); tourVar(currentTour)
              }, Text(?("edit"))) &
              "#remove * " #> SHtml.link("remove", () => {
                stageVar(stage)
                tourVar(currentTour)
                doRemove()
              }, Text(?("remove")))
    })
  }
}