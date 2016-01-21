package com.frustale

import java.time._
import java.time.format.DateTimeFormatter
import java.util.UUID
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import spray.json._

case class Blog(name: String, author: String)

case class Post(title: String, created: LocalDateTime, content: String, modified: Option[LocalDateTime] = None,
                id: String = UUID.randomUUID().toString) {

  def only3Sentences = {
    def findNextSentence(currentPos: Int, numberOfSentences: Int, maxSentences: Int): Int =
      if (numberOfSentences == maxSentences || currentPos == content.length)
        currentPos
      else if (".!?".contains(content.charAt(currentPos)))
        findNextSentence(currentPos + 1, numberOfSentences + 1, maxSentences)
      else
        findNextSentence(currentPos + 1, numberOfSentences, maxSentences)

    copy(content = content.substring(0, findNextSentence(0, 0, 3)))
  }
}

case class Comment(author: String, title: String, content: String, created: LocalDateTime = LocalDateTime.now, id: String = UUID.randomUUID().toString)

trait BlogStore {
  var blog = Blog("Dmitry's blog", "Dmitry")

  var posts = {
    val post = Post("First post", LocalDateTime.now, "Content with four sentences. The second one. The third! The fourth?")
    Map(post.id -> post)
  }

  var comments = Map.empty[String, List[Comment]]

  def blogPosts = BlogPosts(blog.name, blog.author, posts.values.map(_.only3Sentences).toList)

  def createPost(title: String, content: String): Post = {
    val post = Post(title, LocalDateTime.now, content)
    posts += post.id -> post
    post
  }

  def updatePost(id: String, title: Option[String], content: Option[String]): Option[Post] =
    for (old <- posts.get(id))
    yield {
      val post = old.copy(title = title.getOrElse(old.title), content = content.getOrElse(old.content))
      posts += post.id -> post
      post
    }

  def deleteComment(postId: String, commentId: String): Option[Comment] = {
    for {
      comments <- comments.get(postId)
      (found, filtered) = comments.partition(_.id == commentId)
      deletedComment <- found.headOption
    } yield {
      this.comments += postId -> filtered
      deletedComment
    }
  }

  def createComment(postId: String, author: String, title: String, content: String): Comment = {
    val comment = Comment(author, title, content)
    comments += postId -> (comment :: comments.getOrElse(postId, Nil))
    comment
  }

  def deletePost(id: String): Option[Post] =
    for (old <- posts.get(id)) yield {
      posts -= id
      old
    }

  def postComments(id: String): Option[PostComments] =
    for (post <- posts.get(id))
    yield PostComments(blog.name, blog.author, post.title, comments.getOrElse(id, Nil).reverse)
}

case class BlogPosts(name: String, author: String, posts: List[Post])

case class PostComments(blog: String, author: String, title: String, comments: List[Comment])

case class PutPost(title: String, content: String)

case class PutComment(title: String, content: String, author: String)

case class PatchPost(title: Option[String], content: Option[String])

trait BlogProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val dateFormat = new JsonFormat[LocalDateTime] {
    val df = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    def read(json: JsValue): LocalDateTime = LocalDateTime.parse(json.convertTo[String], df)
    def write(dateTime: LocalDateTime): JsValue = JsString(df.format(dateTime))
  }

  implicit val blogFormat = jsonFormat2(Blog)
  implicit val postFormat = jsonFormat5(Post)
  implicit val commentFormat = jsonFormat5(Comment)
  implicit val postsFormat = jsonFormat3(BlogPosts)
  implicit val postComments = jsonFormat4(PostComments)
  implicit val putPostFormat = jsonFormat2(PutPost)
  implicit val patchPost = jsonFormat2(PatchPost)
  implicit val putComment = jsonFormat3(PutComment)
}

/**
 * Entry point.
 * @author Dmitry Dobrynin <dmitry.v.dobrynin@gmail.com>
 *         Created at 20.01.2016 13:53
 */
object BlogServer extends App with BlogProtocol with BlogStore {
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  val route =
    pathEndOrSingleSlash {
      get {
        complete(blogPosts)
      }
    } ~
    put {
      path("/post") {
        entity(as[PutPost]) { post =>
          complete(createPost(post.title, post.content))
        }
      }
    } ~
    path("post" / Segment) { id =>
      get {
        complete(postComments(id))
      } ~
      delete {
        complete(deletePost(id))
      } ~
      patch {
        entity(as[PatchPost]) { patchPost =>
          complete(updatePost(id, patchPost.title, patchPost.content))
        }
      } ~
      path("comment") {
        put {
          entity(as[PutComment]) { pc =>
            complete(createComment(id, pc.author, pc.title, pc.content))
          }
        }
      } ~
      path("comment" / Segment) { commentId =>
        delete {
          complete(deleteComment(id, commentId))
        }
      }
    }

  val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8080)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  io.StdIn.readLine() // for the future transformations
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ ⇒ system.shutdown()) // and shutdown when done
}