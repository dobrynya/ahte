package com.frustale

import java.time._
import java.time.format.DateTimeFormatter
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import spray.json._

case class Blog(name: String, author: String)

case class Post(title: String, created: LocalDateTime, content: String, modified: Option[LocalDateTime] = None) {
  def onlyThreeSentences = {
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

case class Comment(author: String, created: LocalDateTime, content: String)

case class BlogPosts(name: String, author: String, posts: List[Post])

case class PostComments(blog: String, author: String, title: String, comments: List[Comment])

trait BlogProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val dateFormat = new JsonFormat[LocalDateTime] {
    val df = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    def read(json: JsValue): LocalDateTime = LocalDateTime.parse(json.convertTo[String], df)
    def write(dateTime: LocalDateTime): JsValue = JsString(df.format(dateTime))
  }

  implicit val blogFormat = jsonFormat2(Blog)
  implicit val postFormat = jsonFormat4(Post)
  implicit val commentFormat = jsonFormat3(Comment)
  implicit val postsFormat = jsonFormat3(BlogPosts)
  implicit val postComments = jsonFormat4(PostComments)
}

/**
 * Entry point.
 * @author Dmitry Dobrynin <dmitry.v.dobrynin@gmail.com>
 *         Created at 20.01.2016 13:53
 */
object BlogServer extends App with BlogProtocol {
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  val route =
    get {
      pathEndOrSingleSlash {
        complete {
          BlogPosts("Blog Name", "Dmitry",
            List(Post("Fucking test!", LocalDateTime.now(),
              "I don't like this test! Because it sucks. How much time I have? I don't know."))
              .map(_.onlyThreeSentences)
          )
        }
      } ~
      path("posts")
    }

}

  val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8080)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  io.StdIn.readLine() // for the future transformations
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ ⇒ system.shutdown()) // and shutdown when done
}