package models
import sun.security.util.Password
import javax.inject.Inject
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}
import scala.::
import scala.collection.mutable

case class User (
                  var username: String,
                  var password: String,
                  var following: List[String] = List.empty,
                  var subscribedTopics: List[String] = List.empty
                )

@javax.inject.Singleton
class UserDAO @Inject()(){var users:  Seq[User] = Seq(User("anisha", "Anisha@1", List("momo", "admin"), List("#vub", "#scala")),
                                                      User("momo", "Momo@1", List("anisha"), List("#mastersincomputerscience")),
                                                      User("admin","Admin@1", List(),List("#belgium")))

  def validateUser(username: String, password: String):Boolean = {
    users.exists{
      user => user.username.equals(username) &&
        user.password.equals(password)
    }
  }

  def registerNewUser(user: User) :Boolean = {
    if(users.exists(_.username.equals(user.username)))
      false
    else {
      users = users :+ user
      true
    }
  }

  def findUserDetailsByUsername(username: String) = {
    users.find(_.username.equalsIgnoreCase(username))
  }

  def getFollowingUsers(username:String) = {
    val user = users.find(_.username == username).get
    user.following
    }

  def getSubscribedTopics(username:String) = {
    val user = users.find(_.username == username).get
    user.subscribedTopics
  }

}









