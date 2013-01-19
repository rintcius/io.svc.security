package io.svc.security

import scalaz.{Success, Failure, Validation}
import io.svc.security.user.{UsersProvider, UserService, UserWithKey}
import io.svc.security.std.{UsernameNotFound, UserServiceFailure}

/**
 * @author Rintcius Blok
 */
object inMemory {

  trait InMemoryUserService[Key] extends UserService[UserWithKey[Key], Key, UserServiceFailure] {

    val usersProvider: UsersProvider[UserWithKey[Key]]

    lazy val userMap: Map[Key, UserWithKey[Key]] =
      usersProvider.users.foldLeft (Map(): Map[Key, UserWithKey[Key]]) { (map: Map[Key, UserWithKey[Key]], user: UserWithKey[Key]) => addEntry(map, user) }

    override def get(key: Key): Validation[UserServiceFailure, UserWithKey[Key]] = {
      userMap.get(key) map { Success(_) } getOrElse(Failure(UsernameNotFound(key)))
    }
  }

  private def addEntry[Key](m: Map[Key, UserWithKey[Key]], user: UserWithKey[Key]): Map[Key, UserWithKey[Key]] = {
    m + (user.provideKey -> user)
  }

}
