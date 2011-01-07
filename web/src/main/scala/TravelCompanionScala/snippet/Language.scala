package TravelCompanionScala.snippet

import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 20.05.2010
 * Time: 08:53:44
 * To change this template use File | Settings | File Templates.
 */

class Language {
  val languages: Map[String, String] = Map("de_DE" -> "/classpath/images/de.png", "en_US" -> "/classpath/images/en.png")

  def render = {
    ".link" #> languages.map(m => SHtml.link("?locale="+m._1, ()=>(), <img src={m._2}/> ))
  }
}