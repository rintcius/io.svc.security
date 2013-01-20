package io.svc.core

import scalaz.Validation

/**
 * @author Rintcius Blok
 */
object validation {

  /**
   * Trait for handling failures.
   * @tparam I input type
   * @tparam F failure type
   * @tparam O output type
   */
  trait FailureHandler[-I, -F, +O] {
    def handleFailure(in: I, failure: F): O
  }

  /**
   * Trait for handling success.
   * @tparam I input type
   * @tparam S success type
   * @tparam O output type
   */
  trait SuccessHandler[-I, -S, +O] {
    def handleSuccess(in: I, success: S): O
  }

  trait ValidationHandler[-I, -F, -S, +O] extends SuccessHandler[I, S, O] with FailureHandler[I, F, O]

  /**
   * Validator.
   * @tparam I input type
   * @tparam S success type
   * @tparam F failure type
   */
  trait Validator[-I, +S, +F] {
    /**
     * Validate the provided input
     * @param in the input to validate
     * @return the result of the validation
     */
    def validate(in: I): Validation[F, S]
  }


  //TODO looks handy but not used (yet); let's see if we need it; for now using SuccessActionHandler et al
//  trait ValidationFunction[-I, +O] {
//    def validate(in: I): O
//  }

//  trait ValidationProcess[-I, +O] extends ValidationFunction[I,O] {
//    type S
//    type F
//    val validator: Validator[I, S, F]
//    val validationHandler: ValidationHandler[I, F, S, O]
//
//    def validate(in: I): O = validator.validate(in).fold(
//      failure = { f => validationHandler.handleFailure(in, f) },
//      success = { s => validationHandler.handleSuccess(in, s) }
//    )
//  }

  //  trait FailureValidationProcess[I,O] extends ValidationFunction[I,O] {
  //    type Failure
  //    val failureFunction: I => Failure
  //    val failureHandler: FailureHandler[I, Failure, O]
  //    def validate(in: I): O = failureHandler.handleFailure(in, failureFunction(in))
  //  }

  //TODO also add counterpart of traits below (i.e. FailureActionHandler, AlwaysFailureActionHandler & ValidatingFailureActionHandler) ?
  trait SuccessActionHandler[I,S,O] {
    def onSuccess(successAction: (I,S) => O): I => O
  }

  trait AlwaysSuccessActionHandler[I,S,O] extends SuccessActionHandler[I,S,O] {
    val successFunction: I => S

    def onSuccess(successAction: (I,S) => O): I => O = { in: I =>
      successAction(in, successFunction(in))
    }
  }

  trait ValidatingSuccessActionHandler[I,S,O] extends SuccessActionHandler[I,S,O] {
    type Failure
    val validator: Validator[I, S, Failure]
    val failureHandler: FailureHandler[I,Failure,O]

    def onSuccess(successAction: (I,S) => O): I => O = { in: I =>
      validator.validate(in).fold(
        failure = { f => failureHandler.handleFailure(in, f) },
        success = { s => successAction(in, s) }
      )
    }
  }

}
