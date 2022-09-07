package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import javax.inject._
import models.{UserDAO, Global, User}


case class LoginData (username: String, password:String)


@Singleton
class LoginController @Inject()(cc: MessagesControllerComponents, authenticatedUserAction: AuthenticatedUserAction, userDAO: UserDAO) extends MessagesAbstractController(cc) {

  val loginForm = Form(mapping(
    "Username" -> text,
    "Password" -> text
  )(LoginData.apply)(LoginData.unapply)
  )

  def login = Action{implicit request =>
    Ok(views.html.login(loginForm))
  }

  def authorizedLogin = Action {implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.login(formWithErrors)),
      authorizedLogin =>
        if(userDAO.validateUser(authorizedLogin.username, authorizedLogin.password)) {
          Redirect(routes.PostController.displayPost("Date")).withSession(Global.SESSION_USERNAME_KEY -> authorizedLogin.username)
        } else Redirect(routes.LoginController.login).flashing("info" -> "Invalid Username/ Password")
    )
  }

  val registrationForm = Form(mapping(
    "Username" -> nonEmptyText
      .verifying("Username should be greater than 2 characters.", input => lengthIsGreaterThanNCharacters(input, 2))
      .verifying("Username should be less than 20 characters",input => lengthIsLessThanNCharacters(input, 20)),
    "Password" -> nonEmptyText
      .verifying("Password should be greater than 5 characters.", input => lengthIsGreaterThanNCharacters(input, 5))
      .verifying("Password should be less than 20 characters.", input => lengthIsLessThanNCharacters(input, 20))
      .verifying("Password should contain atleast one lower case character.", input => containsLowerCase(input))
      .verifying("Password should contain atleast one upper case character.", input => containsUpperCase(input))
      .verifying("Password should contain atleast one numeric character.", input =>containsNumber(input))
      .verifying("Password should contain atleast one special character.", input => containsSpecialCharacter(input)),
    "Following" -> ignored(List[String]()),
    "SubscribedTopics" -> ignored(List[String]())
  )(User.apply)(User.unapply)
  )

  def newUserLogin = Action{ implicit request =>
    Ok(views.html.registration(registrationForm))
  }

  def newUserRegistration = Action {implicit request =>

    registrationForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.registration(formWithErrors)),
      newUser =>
        if(userDAO.registerNewUser(newUser)) {
          Redirect(routes.PostController.displayPost("Date")).withSession(Global.SESSION_USERNAME_KEY -> newUser.username)
        } else Redirect(routes.LoginController.newUserLogin).flashing("info" -> "User Already exist")
    )
  }

  private def lengthIsGreaterThanNCharacters(input: String, n: Int): Boolean = {
    if (input.length > n) true else false
  }

  private def lengthIsLessThanNCharacters(input: String, n: Int): Boolean = {
    if (input.length < n) true else false
  }

  private def containsLowerCase(input: String): Boolean = {
    input.exists(_.isLower)
  }

  private def containsUpperCase(input: String): Boolean = {
    input.exists(_.isUpper)
  }

  private def containsNumber(input: String): Boolean = {
    input.exists(_.isDigit)
  }

  private def containsSpecialCharacter(input: String): Boolean ={
    !input.matches("^[a-zA-Z0-9]*$")
  }

  def logout = authenticatedUserAction{ implicit request: Request[AnyContent] =>
    // docs: “withNewSession ‘discards the whole (old) session’”
    Redirect(routes.LoginController.login)
      .flashing("info" -> "You are logged out.")
      .withNewSession
  }

}
