package io.svc.security

import scalaz.Validation
import io.svc.core.validation.Validator
import io.svc.core.function.Extractor


/**
 * @author Rintcius Blok
 */
object authentication {

  /**
   * Validator trait that chains a credentials extractor with an authenticationService.
   * @tparam In input type
   * @tparam User type representing a user
   * @tparam F failure type
   */
  trait CredentialsValidator[-In, +User, +F] extends Validator[In, User, F] {
    type Credentials
    val credentialsExtractor: Extractor[In, Credentials, F]
    val authenticationService: AuthenticationService[Credentials, User, F]

    /**
     * Validate the provided input by extracting credentials as defined by credentialsExtractor.
     * If the credentials cannot be extracted a failure is returned. Otherwise the extracted credentials
     * will be used to authenticate against authenticationService and that result will be returned.
     * @param in the input to validate
     * @return the result of the validation
     */
    override def validate(in: In): Validation[F, User] = {
      credentialsExtractor.extract(in) flatMap authenticationService.authenticate
    }
  }

  /**
   * The authentication service
   * @tparam Credentials credentials type
   * @tparam User user type
   * @tparam F failure type
   */
  trait AuthenticationService[-Credentials, +User, +F] {
    def authenticate(credentials: Credentials): Validation[F, User]
  }
}
