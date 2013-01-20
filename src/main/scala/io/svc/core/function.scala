package io.svc.core

import scalaz.Validation

/**
 * @author Rintcius Blok
 */
object function {

  trait Extractor[-In, +Extracted, +F] {

    def extract(in: In): Validation[F, Extracted]
  }


}
