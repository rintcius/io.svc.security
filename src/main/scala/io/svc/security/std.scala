package io.svc.security

import io.svc.security.user._
import io.svc.security.inMemory.InMemoryUserService


/**
 * @author Rintcius Blok
 */
object std {

  trait AuthenticationFailure

  abstract class UserServiceFailure extends AuthenticationFailure
  case class UsernameNotFound[Username](username: Username) extends UserServiceFailure

  case class AuthenticationServiceFailure[A](underlyingError: A) extends AuthenticationFailure

  case class UsernamePasswordCredentials(username: String, password: String)

  class StdInMemoryUserService[Key](users: Seq[UserWithKey[Key]]) extends InMemoryUserService[Key] {
    val usersProvider = new UsersProvider[UserWithKey[Key]] { def users = StdInMemoryUserService.this.users }
  }
}
