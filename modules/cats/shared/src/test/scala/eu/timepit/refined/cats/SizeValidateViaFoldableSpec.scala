package eu.timepit.refined.cats

import cats.data.NonEmptyList
import eu.timepit.refined.collection.MaxSize
import eu.timepit.refined.{W, refineV}
import org.scalacheck.Prop.{AnyOperators, secure}
import org.scalacheck.Properties

class SizeValidateViaFoldableSpec extends Properties("SizeValidateViaFoldableSpec") {

  property("Size validate for NonEmptyList") = secure {
    import eu.timepit.refined.cats.derivation.sizeValidateViaFoldable
    val nel = NonEmptyList.of(1, 2)
    refineV[MaxSize[W.`4`.T]](nel) ?= Right(refineV[MaxSize[W.`4`.T]].unsafeFrom(nel))
  }
}
