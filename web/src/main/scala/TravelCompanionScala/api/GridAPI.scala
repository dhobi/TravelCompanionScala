package TravelCompanionScala.api

/**
 * Created by IntelliJ IDEA.
 * User: pmei
 * Date: 04.06.2010
 * Time: 11:26:26
 * Package: TravelCompanionScala.api
 * Class: GridAPI
 */

import TravelCompanionScala.model.EntityConverter._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.rest.RestHelper
import xml.{Text, Node}
import collection.mutable.Buffer
import net.liftweb.http._
import TravelCompanionScala.model.{UserManagement, Model, Tour}

object tourVar extends RequestVar[Tour](new Tour())

object GridAPI extends RestHelper {

  //This var is needed  for the construction of the xml
  var tourData = Buffer[Node] ()

  //a more performant solution with direct sql query
  val resultOption: Option[java.lang.Long] = Model.createNativeQuery("select count(*) from tours").findOne
  val result = resultOption.getOrElse(new java.lang.Long(100))
  val tourSize = toInt(result)

  var page = 1;
  var rows = 10;


  def getData = {
     for {
      Spage <- S.param("page") ?~ "page parameter missing" ~> 400
      Srows <- S.param("rows") ?~ "row parameter missing" ~> 400
      //the sort order
      Ssord <- S.param("sord") ?~ "row parameter missing" ~> 400
      //which column: ID, Name, Description
      Ssidx <- S.param("sidx") ?~ "row parameter missing" ~> 400

    } yield {
      page = Spage.toInt -1
      rows = Srows.toInt

      //Not posssible to create typesafe query - because only in Java with Java Entity-Types
      //see http://www.ibm.com/developerworks/java/library/j-typesafejpa
       var queryString = "SELECT t from Tour t where not t.owner = :owner order by"

       Ssidx match {
         case "ID" =>  queryString = queryString.concat(" t.id")
         case "Name" =>  queryString = queryString.concat(" t.name")
         case "Description" => queryString = queryString.concat(" t.description")
         case _  => queryString = queryString.concat(" t.id")
       }

       Ssord match {
         case "asc" => queryString = queryString.concat(" ASC")
         case "desc" => queryString = queryString.concat(" DESC")
         case _  =>  queryString = queryString.concat(" ASC")
       }


      val customQuery = Model.createQuery[Tour](queryString)

      //exclude "tours by others" as in named query findTourByOthers
      customQuery.setParameter("owner", UserManagement.currentUser)


      //Not all DBs support FETCH, FIRST, JPA might behave strange when changing the DB
      //see http://troels.arvin.dk/db/rdbms/#select-limit
      customQuery.setFirstResult(page*rows)
      customQuery.setMaxResults(rows)


      val tmpResultList = customQuery.getResultList()
      tourData = tmpResultList.flatMap(tour => bind("tour", tour.toGrid, "name" -> PCData(SHtml.link("view", () => tourVarFromAPI(tour), Text(tour.name)))))
    }
  }

  def listTours = {
    getData
    <rows>
      <page>{ page + 1 }</page>
      <total>{ tourSize/rows}</total>
      <records>{ rows }</records>
      { tourData }
    </rows>
  }


  serve {
    //uses the suffix of the request to determine the response type
    // (GET) /gridapi/tour.xml
    case Req("gridapi" :: "tour" :: _, "xml", GetRequest) => Full(listTours)
    //Works as well: uses the Accept header fto determine the response type
    //case XmlGet("gridapi" :: "tour" :: _, _) => Full(listTours)
  }

}

object tourVarFromAPI extends SessionVar[Tour](new Tour())