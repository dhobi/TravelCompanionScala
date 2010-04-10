/*
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package bootstrap.liftweb

import _root_.java.util.Locale

import _root_.net.liftweb.common.{Box, Empty, Full}
import _root_.net.liftweb.util.{LoanWrapper, LogBoot}
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import TravelCompanionScala.model._
import net.liftweb.http._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // where to search for snippets, views, etc
    LiftRules.addToPackages("TravelCompanionScala")
    // Build SiteMap (used for navigation...)
    val AuthRequired = If(() => UserManagement.loggedIn_?, () => RedirectResponse(UserManagement.loginPageURL))

    // Build SiteMap
    val entries = Menu(Loc("index", "index" :: Nil, "Startseite", LocGroup("main"))) ::
            Menu(Loc("TourView", ("TourView" :: Nil) -> true, "Reise", LocGroup("main"))) ::
            Menu(Loc("blog", "blog" :: Nil, "Blog", LocGroup("main"))) ::
            Menu(Loc("picture", "picture" :: Nil, "Bilder", LocGroup("main"))) :: UserManagement.sitemap

    LiftRules.setSiteMap(SiteMap(entries: _*))

    LiftRules.rewrite.append {
      case RewriteRequest(
      ParsePath(List("TourView", action, id), _, _, _), _, _) =>
        RewriteResponse("TourView" :: action :: Nil, Map("id" -> id))
    }

  }
}

