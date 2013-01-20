package io.svc.security.test.simple

import io.svc.security.authentication._
import scalaz.Validation
import io.svc.security.user.UserWithKey
import io.svc.core.validation._
import io.svc.core.function.Extractor
import scalaz.Success
import scalaz.Failure

/**
 * Simple implementation of [[io.svc.security.authentication]] used for testing.
 *
 * @see io.svc.security.authentication
 * @author Rintcius Blok
 */
object authentication {

  case class SimpleRequest(request: String, username: Option[String] = None, password: Option[String] = None)

  case class SimpleUser(username: String, password: String, email: String) extends UserWithKey[String] {
    val provideKey:String = username
  }

  case class SimpleRequestWithUser()

  trait SimpleResult
  case class SimpleFailureResult(request: SimpleRequest, failure: String) extends SimpleResult
  case class SimpleSuccessResult(request: SimpleRequest, user: SimpleUser) extends SimpleResult

  case class SimpleCredentials(username: String, password: String)

  object SimpleAuthService extends AuthenticationService[SimpleCredentials, SimpleUser, String] {
    override def authenticate(credentials: SimpleCredentials): Validation[String, SimpleUser] = {
      if (credentials.password == ("secret4" + credentials.username)) {
        Success(SimpleUser(credentials.username, credentials.password, credentials.username + "@mymail.com"))
      } else {
        Failure("Invalid credentials for " + credentials.username)
      }
    }
  }

  object SimpleCredentialsExtractor extends Extractor[SimpleRequest, SimpleCredentials, String] {
    override def extract(in: SimpleRequest): Validation[String, SimpleCredentials] = {
      val oCred = for {
        username <- in.username
        password <- in.password
      } yield SimpleCredentials(username, password)
      oCred map (Success(_)) getOrElse Failure("no user or password in request")
    }
  }

  object SimpleInputValidator extends Validator[SimpleRequest, SimpleUser, String] {
    override def validate(request: SimpleRequest): Validation[String, SimpleUser] = {
      val oUser = for {
        username <- request.username
        password <- request.password
      } yield SimpleUser(username, password, username + "@mymail.com")
      oUser map (Success(_)) getOrElse Failure("user or password is not supplied")
    }
  }

  val dummyUser = SimpleUser("dummy", "secret4dummy", "dummy@mymail.com")

  object SimpleAuthFailureHandler extends FailureHandler[SimpleRequest, String, SimpleResult] {
    override def handleFailure(request: SimpleRequest, failure: String) = SimpleFailureResult(request, failure)
  }

  /**
   * 'Full' authentication process, using all traits defined in [[io.svc.security.authentication]].
   */
  val fullAuth = new ValidatingSuccessActionHandler[SimpleRequest, SimpleUser, SimpleResult] {
    type Failure = String
    val validator = new CredentialsValidator[SimpleRequest, SimpleUser, String] {
      type Credentials = SimpleCredentials
      val credentialsExtractor = SimpleCredentialsExtractor
      val authenticationService = SimpleAuthService
    }

    val failureHandler = SimpleAuthFailureHandler

  }


  /**
   * Authentication process with a simpler input validation strategy than fullAuth.
   */
  val auth = new ValidatingSuccessActionHandler[SimpleRequest, SimpleUser, SimpleResult] {
    type Failure = String
    val validator = SimpleInputValidator
    val failureHandler = SimpleAuthFailureHandler
  }

  /**
   * Authentication process that does not do any authentication.
   */
  val noAuth = new AlwaysSuccessActionHandler[SimpleRequest, SimpleUser, SimpleResult] {
    val successFunction = { in: SimpleRequest => dummyUser }
  }

  def simpleActionWithUser(request: SimpleRequest, user: SimpleUser): SimpleResult = {
    if (request.request.toLowerCase.contains("hello")) {
      SimpleSuccessResult(request, user)
    } else {
      SimpleFailureResult(request, "request does not contain hello")
    }
  }

  val simpleAuthAction = auth.onSuccess(simpleActionWithUser)

  val simpleFullAuthAction = fullAuth.onSuccess(simpleActionWithUser)

  val simpleNoAuthAction = noAuth.onSuccess(simpleActionWithUser)
}
