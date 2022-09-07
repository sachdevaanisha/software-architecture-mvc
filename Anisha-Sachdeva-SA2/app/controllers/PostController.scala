package controllers

import models.{Comment, Global, Post, PostDAO, UserDAO, User, LikeAndDelete, Search, Follow, Subscribe}
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import java.nio.file.{Files, Paths}

import java.util.{Calendar}
import javax.inject._


class PostController @Inject()(cc: MessagesControllerComponents, postDao: PostDAO,
                               authenticatedUserAction: AuthenticatedUserAction,
                               userDao: UserDAO) extends MessagesAbstractController(cc)
                               with play.api.i18n.I18nSupport {

  def displayPost(sortingOption: String) = authenticatedUserAction {implicit request =>
    val usernameOption = request.session.get(Global.SESSION_USERNAME_KEY)
    usernameOption.map{
      username =>
        val followingUsers = userDao.getFollowingUsers(username)
        var followingPostsByUsers = List[Post]()
        for (eachFollowing <- followingUsers) {
          followingPostsByUsers = followingPostsByUsers ::: postDao.findByUsername(eachFollowing)
        }

        var followingUsersAndOwnPosts = postDao.findByUsername(username) ::: followingPostsByUsers

        val subscribedTopics = userDao.getSubscribedTopics(username)
        var followingPostsByTopics = List[Post]()
        for(eachTopic <- subscribedTopics) {
          followingPostsByTopics = followingPostsByTopics ::: postDao.findByTopic(eachTopic)
        }

        var withoutDuplicatePostList = followingUsersAndOwnPosts
        for(eachpost <- followingPostsByTopics){
          if(!followingUsersAndOwnPosts.contains(eachpost)){
            withoutDuplicatePostList = withoutDuplicatePostList :+ eachpost
          }
        }

        if(sortingOption.equalsIgnoreCase("Date")) {
          val sortByDate = withoutDuplicatePostList.sortBy(_.date).reverse
          Ok(views.html.postView.explore(sortByDate, commentForm, searchForm, likeForm))

        } else {
          val sortByLikes = withoutDuplicatePostList.sortBy(post => post.getLikeCount).reverse
          Ok(views.html.postView.explore(sortByLikes, commentForm, searchForm, likeForm))
        }

    }.getOrElse(Redirect(routes.LoginController.login))
  }

  val commentForm = Form(mapping(
    "postId" -> longNumber,
    "commentedBy" -> nonEmptyText,
    "commentText" -> nonEmptyText,
    "commentImage" -> ignored(""),
  )(Comment.apply)(Comment.unapply))

  def addComment(postId: Long, requestURI: String) = authenticatedUserAction(parse.multipartFormData)
  {implicit request =>
    postDao.findPostByID(postId).map{
      post =>
        val newCommentForm = commentForm.bindFromRequest()
        newCommentForm.fold(
          hasErrors = { formWithErrors: Form[Comment] =>
            BadRequest("Invalid fields! Please try again.")
          } ,
          success = { comment: Comment =>
            ( comment.commentImage = uploadImage("commentImages"),
              post.comments = post.comments :+ comment
              )
            Redirect(requestURI)
          }
        )
    }.getOrElse(NotFound)
  }

  val postForm = Form(mapping(
    "postId" -> ignored(1L),
    "description" -> text,
    "date" -> ignored(Calendar.getInstance().getTime),
    "postUploadedBy" -> ignored(""),
    "imageFileName" -> ignored(""),
    "like" -> ignored(List[String]()),
    "comments" -> ignored(List[Comment]()),
    "topic" -> nonEmptyText
      .verifying("The topic should start with a hashtag (#). Please try again", topic => containsHashtag(topic))
  )(Post.apply)(Post.unapply))

  def showNewPost = authenticatedUserAction {implicit request =>
    Ok(views.html.postView.addNewPost(postForm))
  }

  def addNewPost = authenticatedUserAction(parse.multipartFormData){implicit request =>
    val newPostForm = postForm.bindFromRequest()
    val username = request.session.get(Global.SESSION_USERNAME_KEY).get
    newPostForm.fold(
      hasErrors = { formWithErrors: Form[Post] =>
        BadRequest(views.html.postView.addNewPost(formWithErrors))
      } ,
      success = { post: Post =>
        post.postId = postDao.generateNewPostId
        post.postUploadedBy = username
        post.imageFileName = uploadImage("postImages")
        postDao.posts = postDao.posts :+ post
        Redirect(routes.PostController.displayPost("Date"))
      }
    )
  }

  def uploadImage(imageDirectoryOption: String)(implicit request: Request[MultipartFormData[play.api.libs.Files.TemporaryFile]]):
                                String = {
    val username = request.session.get(Global.SESSION_USERNAME_KEY).get
    var directoryPath = ""
    request.body.file("image").map{ image =>
      val imageName = Paths.get(image.filename).getFileName

      var pathToImage = Paths.get(s"public/images/$username/$imageDirectoryOption/$imageName")

      Files.createDirectories(pathToImage.getParent)
      image.ref.copyTo(pathToImage, replace = true)

      directoryPath = pathToImage.getFileName.toString
    }
    directoryPath
  }

  def showOwnProfile = authenticatedUserAction{implicit request =>
    val usernameOption = request.session.get(Global.SESSION_USERNAME_KEY)
    usernameOption.map{
      username =>
        Redirect(routes.PostController.sortedUserProfilePosts(username, "Date"))
    }.getOrElse(Redirect(routes.PostController.displayPost("Date")))
  }

  val searchForm = Form(mapping(
    "searchKeyword" -> nonEmptyText
  )(Search.apply)(Search.unapply))

  val searchRelatedPosts = authenticatedUserAction{implicit request =>
    val newSearchForm = searchForm.bindFromRequest()
    newSearchForm.fold(
      hasErrors = { formWithErrors: Form[Search] =>
        BadRequest("Invalid fields! Please try again.")
      } ,
      success = { search: Search =>

        val usernameMatch = search.searchKeyword.startsWith("@")
        val topicMatch = search.searchKeyword.startsWith("#")
        val usernameOrTopicKeyword = search.searchKeyword.substring(1)

        if(usernameMatch){
          Redirect(routes.PostController.sortedUserProfilePosts(usernameOrTopicKeyword, "Date"))
        } else if (topicMatch){
          Redirect(routes.PostController.sortedTopicPosts(search.searchKeyword, "Date"))
        } else {
          Redirect(routes.PostController.displayPost("date"))
            .flashing("info" -> s"${search.searchKeyword} is an invalid keyword. Please enter a valid keyword either starting from '@' to search for a user profile or '#' to search for a topic.")
        }
      }
    )
  }

  def sortedUserProfilePosts(username: String, sortingOption: String) = authenticatedUserAction { implicit request =>
    val allPostsByAUser = postDao.findByUsername(username)
    if (userDao.findUserDetailsByUsername(username).nonEmpty) {
      if (sortingOption.equalsIgnoreCase("Date")) {
        val sortByDate = allPostsByAUser.sortBy(_.date).reverse
        Ok(views.html.postView.userProfile(username, sortByDate, commentForm, likeForm, followUserForm, deleteForm))
      } else {
        val sortByLikes = allPostsByAUser.sortBy(post => post.getLikeCount).reverse
        Ok(views.html.postView.userProfile(username, sortByLikes, commentForm, likeForm, followUserForm, deleteForm))
      }
    } else Redirect(routes.PostController.displayPost("date")).flashing("info" -> s"$username not found!! Try again.")
  }

  def sortedTopicPosts(topic: String, sortingOption: String) = authenticatedUserAction { implicit request =>
    val allPostsOfOneTopic = postDao.findByTopic(topic)

    if (allPostsOfOneTopic.nonEmpty) {

      if (sortingOption.equalsIgnoreCase("Date")) {
        val sortByDate = allPostsOfOneTopic.sortBy(_.date).reverse
        Ok(views.html.postView.topics(topic, sortByDate, commentForm, likeForm, subscribeTopicForm))
      } else {
        val sortByLikes = allPostsOfOneTopic.sortBy(post => post.getLikeCount).reverse
        Ok(views.html.postView.topics(topic, sortByLikes, commentForm, likeForm, subscribeTopicForm))
      }

    } else Redirect(routes.PostController.displayPost("date")).flashing("info" -> s"No posts for the topic $topic exists.")

  }

  val likeForm = Form(mapping(
    "postId" -> longNumber
  )(LikeAndDelete.apply)(LikeAndDelete.unapply))

  def addLike(postId: Long, requestURI: String) = authenticatedUserAction { implicit request =>
    postDao.findPostByID(postId).map {
      post =>
        val newLikeForm = likeForm.bindFromRequest()
        newLikeForm.fold(
          hasErrors = { formWithErrors: Form[LikeAndDelete] =>
            BadRequest("Invalid fields! Please try again.")
          },
          success = { like =>
              val postLikedBy = request.session.get(Global.SESSION_USERNAME_KEY).get

            if(!post.like.contains(postLikedBy)) {
              post.like = post.like :+ postLikedBy
              Redirect(requestURI: String)
            } else {
              Redirect(requestURI: String).flashing("info" -> "Post is already liked by the user")
            }

          }
        )
    }.getOrElse(NotFound)
  }

  val followUserForm = Form(mapping(
    "followUsername" -> text
  )(Follow.apply)(Follow.unapply))

  def addFollowing(followingUser: String) = authenticatedUserAction{implicit request =>
    val username = request.session.get(Global.SESSION_USERNAME_KEY).get
    userDao.findUserDetailsByUsername(username).map{
      user =>
        val newFollowingUserForm = followUserForm.bindFromRequest()
        newFollowingUserForm.fold(
          hasErrors = { formWithErrors: Form[Follow] =>
            BadRequest("Invalid fields! Please try again.")
          },
          success = { follow =>
            if(username == followingUser){
              Redirect(routes.PostController.displayPost("date")).flashing("info" -> "You can not follow yourself. Your own posts will always appear in Explore. Enjoy :)")
            } else {
              if(user.following.contains(followingUser)){
                Redirect(routes.PostController.displayPost("date")).flashing("info" -> s"You are already following $followingUser")
              } else {
                user.following = user.following :+ followingUser
                Redirect(routes.PostController.displayPost("date")).flashing("info" -> s"You are now following $followingUser :)")
              }
            }
          }
        )
    }.getOrElse(NotFound)
  }

  val subscribeTopicForm = Form(mapping(
    "followTopic" -> text
  )(Subscribe.apply)(Subscribe.unapply))

  def addTopic(followingTopic: String) = authenticatedUserAction{implicit request =>
    val username = request.session.get(Global.SESSION_USERNAME_KEY).get
    userDao.findUserDetailsByUsername(username).map{
      user =>
        val newSubscribeTopicForm = subscribeTopicForm.bindFromRequest()
        newSubscribeTopicForm.fold(
          hasErrors = { formWithErrors: Form[Subscribe] =>
            BadRequest("Invalid fields! Please try again.")
          },
          success = { follow =>
            if(user.subscribedTopics.contains(followingTopic)){
              Redirect(routes.PostController.displayPost("date")).flashing("info" -> s"You are already subscribed to $followingTopic")
              } else {
              user.subscribedTopics = user.subscribedTopics :+ followingTopic
              Redirect(routes.PostController.displayPost("date")).flashing("info" -> s"You are now subscribed to $followingTopic :)")
              }
          }
        )
    }.getOrElse(NotFound)
  }

  val deleteForm = Form(mapping(
    "postId" -> longNumber
  )(LikeAndDelete.apply)(LikeAndDelete.unapply))

  def deletePost(postId: Long) = authenticatedUserAction{implicit request =>
    val newDeleteForm = deleteForm.bindFromRequest()
    newDeleteForm.fold(
      hasErrors = { formWithErrors: Form[LikeAndDelete] =>
        BadRequest("Invalid fields! Please try again.")
      },
      success = { delete =>
        postDao.deletePost(postId)
        Redirect(routes.PostController.showOwnProfile)
      }
    )


  }

  private def containsHashtag(topic: String) = {
    topic.startsWith("#")
  }

}
