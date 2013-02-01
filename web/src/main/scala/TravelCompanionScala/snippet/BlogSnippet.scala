package TravelCompanionScala.snippet

import scala.xml.NodeSeq

import net.liftweb._
import common.{Full, Empty}
import http._
import http.js.jquery.JqJsCmds
import http.js.JsCmd
import S._
import util._
import Helpers._
import JqJsCmds._

import TravelCompanionScala.model._
import java.text.SimpleDateFormat
import scala.collection.JavaConversions._
import TravelCompanionScala.controller._
import TravelCompanionScala.controller.DeleteComment
import TravelCompanionScala.controller.DeleteEntry
import TravelCompanionScala.controller.EditEntry
import xml.Text
import TravelCompanionScala.controller.AddComment
import TravelCompanionScala.controller.AddEntry

/**
 * Created by IntelliJ IDEA.
 * User: Ralf Muri
 * Date: 19.04.2010
 * Time: 09:02:05
 * To change this template use File | Settings | File Templates.
 */

// Set up a requestVar to track the STAGE object for edits and adds
object blogEntryVar extends RequestVar[BlogEntry](new BlogEntry())

object commentVar extends RequestVar[Comment](new Comment())

object myJqRemove {
  def apply(uid: String): JsCmd = new Remove(uid)
}

class Remove(uid: String) extends JsCmd {
  def toJsCmd = "try{jQuery(" + ("#" + uid).encJs + ").remove();} catch (e) {}"
}

class BlogSnippet {

  /* Blog as single webpage Application */

  val entriesDivId = "entriesList"
  val entryFormDivId = "addEntryForm"
  val newEntryLink = "newEntryLink"
  val entryErrorDivId = "addEntryErrors"
  val blogEntryDivId = "blogEntry"
  val commentDivId = "comment"
  val commentErrorDivId = "commentError"
  val commentFormDivId = "commentForm"

  def getErrorDiv(divIdPrefix: String) = <div id={divIdPrefix} style="display: none;">
    <div class="lift:msgs">
        <span class="lift:error_msg"/>
    </div>
  </div>

  def render = {

    val entryTemplate = Templates("blog" :: "_chooseEntry" :: Nil).getOrElse(NodeSeq.Empty)
    val entryFormTemplate = Templates("blog" :: "_chooseForm" :: Nil).getOrElse(NodeSeq.Empty)
    val commentsTemplate = Templates("blog" :: "_chooseComments" :: Nil).getOrElse(NodeSeq.Empty)
    val commentFormTemplate = Templates("blog" :: "_chooseCommentForm" :: Nil).getOrElse(NodeSeq.Empty)


    def doEditBlogEntry(entry: BlogEntry): JsCmd = {
      val save = () => {
        if (validator.is_valid_entity_?(entry)) {
          val merged = Model.mergeAndFlush(entry)
          BlogCache.cache ! EditEntry(merged)
          JqSetHtml(blogEntryDivId + entry.id, listEntries(entryTemplate, List(merged)))
        } else {
          Show(entryErrorDivId)
        }
      }
      val cancel = () => Hide(entryErrorDivId) & JqSetHtml(blogEntryDivId + entry.id, listEntries(entryTemplate, List(Model.getReference(classOf[BlogEntry], entry.id))))
      JqSetHtml(blogEntryDivId + entry.id, getEntryForm(entry, entryFormTemplate, save, cancel))
    }

    def doComments(entry: BlogEntry): JsCmd = {

      def removeComment(c: Comment): JsCmd = {
        val mergedc = Model.merge(c)
        Model.removeAndFlush(mergedc)
        BlogCache.cache ! DeleteComment(entry)
        JqSetHtml(commentDivId + entry.id, renderComments())
      }

      def renderComments() = {
        val merged = Model.merge(entry)
        Model.refresh(merged)
        (".comment *" #> merged.comments.map(c =>
          ".comment_member" #> c.member.name &
            ".comment_dateCreated" #> new SimpleDateFormat("dd.MM.yyyy HH:mm").format(c.dateCreated) &
            ".comment_content" #> c.content &
            ".comment_remove" #> {
              if ((c.member == UserManagement.currentUser) || (entry.owner == UserManagement.currentUser))
                SHtml.a(() => removeComment(c), Text(?("remove")))
              else
                NodeSeq.Empty
            }
        )).apply(commentsTemplate)
      }

      def renderNewCommentForm(): NodeSeq = {
        def doSaveComment(c: Comment): JsCmd = {
          if (validator.is_valid_entity_?(c)) {
            val merged = Model.merge(entry)
            merged.comments.add(c)
            Model.mergeAndFlush(merged)
            BlogCache.cache ! AddComment(merged)
            Hide(commentErrorDivId) & JqSetHtml(commentDivId + entry.id, renderComments()) & JqSetHtml(commentFormDivId + entry.id, renderNewCommentForm())
          } else {
            Show(commentErrorDivId)
          }
        }

        val newComment = new Comment
        newComment.blogEntry = entry
        newComment.member = UserManagement.currentUser
        newComment.dateCreated = TimeHelpers.now

        (".blog_error *" #> getErrorDiv(commentErrorDivId) &
          ".blog_newComment *" #> SHtml.textarea(newComment.content, newComment.content = _) &
          ".blog_submit *" #> SHtml.ajaxSubmit(?("save"), () => doSaveComment(newComment)) &
          ".blog_cancel *" #> {
            SHtml.a(() => Hide(commentErrorDivId) &
              JqSetHtml(blogEntryDivId + entry.id, listEntries(entryTemplate, List(entry))), Text(?("cancel")), "class" -> "button")
          })(SHtml.ajaxForm(commentFormTemplate))
      }

      JqSetHtml(commentDivId + entry.id, renderComments()) & JqSetHtml(commentFormDivId + entry.id, renderNewCommentForm())
    }

    def doRemoveBlogEntry(entry: BlogEntry): JsCmd = {
      val e = Model.merge(entry)
      Model.removeAndFlush(e)
      BlogCache.cache ! DeleteEntry(e)
      myJqRemove(blogEntryDivId + entry.id)
    }

    def listEntries(html: NodeSeq, entries: List[BlogEntry]): NodeSeq = {
      def belongsTo(entry: BlogEntry): NodeSeq = {
        if (entry.tour == null) {
          NodeSeq.Empty
        } else {
          Text(?("blog.belongsTo") + " ") ++ SHtml.link("/tour/view", () => tourVar(entry.tour), Text(entry.tour.name))
        }
      }

      //create NodeSeq => NodeSeq (CSSBindFunc)
      (".entries" #> entries.map(entry =>
        "#defaultId [id]" #> Text(blogEntryDivId + entry.id) &
          ".entry_title *" #> entry.title &
          ".entry_tour *" #> belongsTo(entry) &
          ".entry_content *" #> entry.content &
          ".entry_edit *" #> SHtml.a(() => doEditBlogEntry(entry), Text(?("edit"))) &
          ".entry_comments *" #> SHtml.a(() => doComments(entry), Text(?("blog.comments"))) &
          ".entry_remove *" #> SHtml.a(() => doRemoveBlogEntry(entry), Text(?("remove"))) &
          ".entry_preview *" #> entry.content.substring(0, scala.math.min(entry.content.length, 50)) &
          ".entry_readOn *" #> SHtml.link("/blog/view", () => blogEntryVar(entry), Text(?("blog.readOn"))) &
          ".entry_lastUpdated *" #> new SimpleDateFormat("dd.MM.yyyy HH:mm").format(entry.lastUpdated) &
          ".creator *" #> entry.owner.name &
          "#commentsId [id]" #> Text(commentDivId + entry.id) &
          "#commentFormId [id]" #> Text(commentFormDivId + entry.id)
      )).apply(entryTemplate) //apply NodeSeq (execution of function NodeSeq => NodeSeq)
    }

    def getEntryForm(e: BlogEntry, html: NodeSeq, submitFunc: () => JsCmd, cancelFunc: () => JsCmd): NodeSeq = {
      val tours = Model.createNamedQuery[Tour]("findTourByOwner").setParams("owner" -> UserManagement.currentUser).findAll.toList
      val choices = List("" -> ("- " + S.?("none") + " -")) ::: tours.map(tour => (tour.id.toString -> tour.name)).toList

      (".entry_error *" #> getErrorDiv(entryErrorDivId) &
        ".entry_title *" #> SHtml.text(e.title, e.title = _) &
        ".entry_content *" #> SHtml.textarea(e.content, e.content = _) &
        ".entry_tour *" #> {
          SHtml.select(choices,
            if (e.tour == null)
              Empty
            else
              Full(e.tour.id.toString), (tourId: String) => {
              if (tourId != "")
                e.tour = Model.getReference(classOf[Tour], tourId.toLong)
              else
                e.tour = null
            })
        } &
        ".entry_owner *" #> SHtml.text(e.owner.name, e.owner.name = _) &
        ".entry_submit *" #> SHtml.ajaxSubmit(?("save"), submitFunc) &
        ".entry_cancel *" #> SHtml.a(cancelFunc, Text(?("cancel")), "class" -> "button")
        )(SHtml.ajaxForm(html))
    }

    def doNewEntry() = {
      def save(entry: BlogEntry): JsCmd = {
        if (validator.is_valid_entity_?(entry)) {
          val merged = Model.mergeAndFlush(entry)
          BlogCache.cache ! AddEntry(merged)
          Hide(entryErrorDivId) &
            Hide(entryFormDivId) &
            Show(newEntryLink) &
            AppendHtml(entriesDivId, listEntries(entryTemplate, List(merged)))
        } else {
          Show(entryErrorDivId)
        }
      }

      def addEntryForm(html: NodeSeq): NodeSeq = {
        val e = new BlogEntry
        e.owner = UserManagement.currentUser
        e.lastUpdated = TimeHelpers.now
        getEntryForm(e, html, () => save(e), () => Hide(entryFormDivId) & Hide(entryErrorDivId) & Show(newEntryLink))
      }

      SHtml.a(
        () => Hide(newEntryLink) & Show(entryFormDivId) & JqSetHtml(entryFormDivId, addEntryForm(entryFormTemplate)),
        Text(?("blog.addEntry")),
        "class" -> "button")
    }

    def listOwnEntries(html: NodeSeq): NodeSeq = {
      val entries = Model.createNamedQuery[BlogEntry]("findEntriesByOwner").setParams("owner" -> UserManagement.currentUser).findAll.toList
      listEntries(html, entries)
    }

    "#entriesList *" #> <div id={entriesDivId}>
      {listOwnEntries(entryTemplate)}
    </div> &
      "#template *" #> NodeSeq.Empty &
      "#newEntry *" #> <div id={newEntryLink} class="content">
        {doNewEntry()}
      </div> &
      "#newEntryForm *" #> <div id={entryFormDivId}></div>
  }


  /* Blog as traditional multi page application */

  def blogEntry = blogEntryVar.is

  def removeBlogEntry(entry: BlogEntry) {
    val e = Model.merge(entry)
    Model.remove(e)
    BlogCache.cache ! DeleteEntry(e)
    S.redirectTo("/blog/list")
  }

  def editBlogEntry(html: NodeSeq): NodeSeq = {
    def doEdit() {
      if (validator.is_valid_entity_?(blogEntry)) {
        val newEntry = Model.mergeAndFlush(blogEntry)
        BlogCache.cache ! AddEntry(newEntry)
        S.redirectTo("/blog/list")
      }
    }

    val currentEntry = blogEntry

    currentEntry.owner = UserManagement.currentUser
    currentEntry.lastUpdated = TimeHelpers.now

    val tours = Model.createNamedQuery[Tour]("findTourByOwner").setParams("owner" -> UserManagement.currentUser).findAll.toList
    val choices = List("" -> ("- " + S.?("none") + " -")) ::: tours.map(tour => (tour.id.toString -> tour.name)).toList

    bind("entry", html,
      "title" -> SHtml.text(currentEntry.title, currentEntry.title = _),
      "content" -> SHtml.textarea(currentEntry.content, currentEntry.content = _),
      "tour" -> SHtml.select(choices, if (currentEntry.tour == null) Empty else Full(currentEntry.tour.id.toString), (tourId: String) => {
        if (tourId != "") currentEntry.tour = Model.getReference(classOf[Tour], tourId.toLong) else currentEntry.tour = null
      }),
      "owner" -> SHtml.text(currentEntry.owner.name, currentEntry.owner.name = _),
      "submit" -> SHtml.submit(?("save"), () => {
        blogEntryVar(currentEntry)
        doEdit()
      }))
  }

  def showEntry = {
    val currentEntry = blogEntry
    listEntries(List(blogEntry))
  }

  def listEntries(entries: List[BlogEntry]) = {


    ".blog" #> entries.map(entry => {
      "#title *" #> entry.title &
        "#tour *" #> {
          if (entry.tour == null) {
            NodeSeq.Empty
          } else {
            Text(?("blog.belongsTo") + " ") ++ SHtml.link("/tour/view", () => tourVar(entry.tour), Text(entry.tour.name))
          }
        } &
        "#content *" #> entry.content &
        "#edit *" #> SHtml.link("/blog/edit", () => blogEntryVar(entry), Text(?("edit"))) &
        "#comments *" #> SHtml.link("/blog/view", () => blogEntryVar(entry), Text(?("blog.comments"))) &
        "#remove *" #> SHtml.link("/blog/remove", () => removeBlogEntry(entry), Text(?("remove"))) &
        "#preview *" #> entry.content.substring(0, scala.math.min(entry.content.length, 50)) &
        "#readOn *" #> SHtml.link("/blog/view", () => blogEntryVar(entry), Text(?("blog.readOn"))) &
        "#lastUpdated *" #> new SimpleDateFormat("dd.MM.yyyy HH:mm").format(entry.lastUpdated) &
        "#creator *" #> entry.owner.name
    })
  }


  def showBlogEntriesFromTour = {
    val currentTour = tourVar.is
    val entries = Model.createNamedQuery[BlogEntry]("findEntriesByTour").setParams("tour" -> currentTour).findAll.toList
    listEntries(entries)
  }

  def listOtherEntries = {
    val entries = Model.createNamedQuery[BlogEntry]("findEntriesByOthers").setParams("owner" -> UserManagement.currentUser).findAll.toList
    listEntries(entries)
  }

  def listOwnEntries = {
    val entries = Model.createNamedQuery[BlogEntry]("findEntriesByOwner").setParams("owner" -> UserManagement.currentUser).findAll.toList
    listEntries(entries)
  }

  def addComment() = {
    def doAdd(c: Comment) = {
      if (validator.is_valid_entity_?(c))
        Model.mergeAndFlush(c)
    }

    val currentEntry = blogEntry
    val newComment = new Comment
    newComment.blogEntry = blogEntry
    newComment.member = UserManagement.currentUser
    newComment.dateCreated = TimeHelpers.now

    "#comment_content" #> SHtml.textarea(newComment.content, newComment.content = _) &
      "type=submit" #> SHtml.submit(?("save"), () => {
        blogEntryVar(currentEntry)
        doAdd(newComment)
      })
  }

  def doRemoveComment(comment: Comment) {
    val c = Model.merge(comment)
    Model.remove(c)
    S.redirectTo("/blog/view", () => blogEntryVar(c.blogEntry))
  }

  def showComments = {
    val comments = Model.createNamedQuery[Comment]("findCommentsByEntry").setParams("entry" -> blogEntry).findAll.toList

    ".blog" #> comments.map(comment => {
      "#member *" #> comment.member.name &
        "#dateCreated *" #> new SimpleDateFormat("dd.MM.yyyy HH:mm").format(comment.dateCreated) &
        "#content *" #> comment.content &
        "#link_remove *" #> {
          if ((comment.member == UserManagement.currentUser) || (blogEntry.owner == UserManagement.currentUser)) {
            SHtml.link("remove", () => {
              blogEntryVar(comment.blogEntry)
              doRemoveComment(comment)
            }, Text(?("remove")))
          } else {
            NodeSeq.Empty
          }
        }

    }
    )
  }
}