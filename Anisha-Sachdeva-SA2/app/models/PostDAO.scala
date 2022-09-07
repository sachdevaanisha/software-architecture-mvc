package models

import sun.security.util.Password
import javax.inject.Inject
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}
import scala.::
import scala.collection.mutable

object PostDAO {
  val defaultDateOfPost = new SimpleDateFormat("dd/MM/y")

  def getFileTypes(): List[String] = List("image/jpeg", "image/png")
  def getFileExtensions(): List[String] = List(".jpg", ".jpeg", ".png")
}

case class Post(
                 var postId: Long,
                 var description: String,
                 var date: Date = Calendar.getInstance().getTime,
                 var postUploadedBy: String,
                 var imageFileName : String,
                 var like: List[String] = List.empty,
                 var comments: List[Comment] = List.empty,
                 var topic: String
               ){
  def getLikeCount= {
    like.size
  }

  def getLikedByList(username: String) = {
    like.contains(username)
  }

}

@javax.inject.Singleton
class PostDAO @Inject()() {

  var posts: Seq[Post] = Seq(
    Post(1, "This is my first MVC project", PostDAO.defaultDateOfPost.parse("12/08/2022"),
      "anisha","sios.png", List("momo"), List(Comment(1, "momo", "Wow, this is my first comment","")), "#scala"),
    Post(2, "Anisha's  second post", PostDAO.defaultDateOfPost.parse("13/08/2022"),
      "anisha","VUB.jpg", List("momo", "anisha", "admin"),
      List(Comment(2, "anisha", "I love VUB",""), Comment(2, "momo", "Me too!!","")),"#vub"),
    Post(3, "Anisha's third explore", PostDAO.defaultDateOfPost.parse("12/08/2022"),
      "anisha","Anisha.jpeg", List(), List(),"#playframework"),
    Post(4, "Momo's first post", PostDAO.defaultDateOfPost.parse("12/08/2022"),
      "momo","Momo.jpeg", List("anisha"), List(Comment(4,"anisha", "Cutie!!","cute.jpg")),"#softwarearchitecture"),
    Post(5, "Momo's second explore", PostDAO.defaultDateOfPost.parse("14/08/2022"),
      "momo","Momo.jpeg", List("admin"),
      List(Comment(5, "admin", "There should be one comment from admin also","")),"#mastersincomputerscience"),
    Post(6, "Admin first explore", PostDAO.defaultDateOfPost.parse("12/08/2022"),
      "admin","Admin.png",List(), List(),"#belgium")
  )

  var currentPostId = 7

  def generateNewPostId = {
    currentPostId = currentPostId +1
    currentPostId
  }

  def findByUsername(username: String) = {
    posts.filter(_.postUploadedBy.equalsIgnoreCase(username)).toList
  }

  def findPostByID(postID: Long) = {
    posts.find(_.postId == postID)
  }

  def findByTopic(topic: String) = {
    posts.filter(_.topic.equalsIgnoreCase(topic)).toList
  }

  def deletePost(postID: Long) = {
    posts = posts.filterNot(_.postId == postID)
  }

}
