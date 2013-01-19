package io.svc.security

import scalaz.{Success, Failure, Validation}
import io.svc.security.authentication.AuthenticationService
import io.svc.security.std.{AuthenticationServiceFailure, AuthenticationFailure, UsernamePasswordCredentials}

/**
 * @author Rintcius Blok
 */
object user {

  trait UserProvider[+User] {
    def user: User
  }

  trait UserWithKey[+Key] {
    def provideKey: Key
  }

  trait UserService[+User, Key, +Failure] {
    def get(key: Key): Validation[Failure, User]
  }

  trait CredentialsValidator[+User, +Credentials, +Failure] {
    def validate[A >: User, B >: Credentials](user: A, credentials: B): Validation[Failure, A]
  }

  trait UsernamePasswordCredentialsAuthenticationService[User] extends AuthenticationService[UsernamePasswordCredentials, User, AuthenticationFailure] {
    val userService: UserService[User, String, AuthenticationFailure]
    val credentialsValidator: CredentialsValidator[User, UsernamePasswordCredentials, AuthenticationFailure]
    override def authenticate(credentials: UsernamePasswordCredentials): Validation[AuthenticationFailure, User] = {
      val oUser: Validation[io.svc.security.std.AuthenticationFailure,User] = userService.get(credentials.username)
      oUser match {
        case Failure(f) => Failure(AuthenticationServiceFailure(f))
        case Success(user) => credentialsValidator.validate[User, UsernamePasswordCredentials](user: User, credentials: UsernamePasswordCredentials)
      }
    }
  }

  trait UsersProvider[+User] {
    def users: Seq[User]
  }
}
