package io.svc.security

import scalaz.{Success, Failure, Validation}
import io.svc.security.authentication.AuthenticationService
import io.svc.security.std.{AuthenticationServiceFailure, AuthenticationFailure, UsernamePasswordCredentials}

/**
 * @author Rintcius Blok
 */
object user {

  trait UserWithKey[+Key] {
    def provideKey: Key
  }

  trait UserService[+User, Key, +Failure] {
    def get(key: Key): Validation[Failure, User]
  }

  trait CredentialsVerifier[+User, +Credentials, +Failure] {
    def verify[A >: User, B >: Credentials](user: A, credentials: B): Validation[Failure, A]
  }

  trait UsernamePasswordCredentialsAuthenticationService[User] extends AuthenticationService[UsernamePasswordCredentials, User, AuthenticationFailure] {
    val userService: UserService[User, String, AuthenticationFailure]
    val credentialsVerifier: CredentialsVerifier[User, UsernamePasswordCredentials, AuthenticationFailure]
    override def authenticate(credentials: UsernamePasswordCredentials): Validation[AuthenticationFailure, User] = {
      val oUser: Validation[io.svc.security.std.AuthenticationFailure,User] = userService.get(credentials.username)
      oUser match {
        case Failure(f) => Failure(AuthenticationServiceFailure(f))
        case Success(user) => credentialsVerifier.verify[User, UsernamePasswordCredentials](user: User, credentials: UsernamePasswordCredentials)
      }
    }
  }

  trait UsersProvider[+User] {
    def users: Seq[User]
  }
}
