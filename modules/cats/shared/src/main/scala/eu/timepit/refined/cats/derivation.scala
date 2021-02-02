package eu.timepit.refined.cats

import cats.{Contravariant, Foldable, MonadError}
import cats.syntax.foldable._
import eu.timepit.refined.api.{RefType, Validate}
import eu.timepit.refined.collection.Size
import eu.timepit.refined.internal.Resources

object derivation extends DerivationInstances

trait DerivationInstances {

  /**
   * `G` instance for refined types derived via `Contravariant[G]`
   * that delegates to the `G` instance of the base type.
   *
   * Typical examples for `G` are encoders.
   */
  implicit def refTypeViaContravariant[F[_, _], G[_], T, P](implicit
      c: Contravariant[G],
      rt: RefType[F],
      gt: G[T]
  ): G[F[T, P]] = c.contramap(gt)(rt.unwrap)

  /**
   * `G` instance for refined types derived via `MonadError[G, String]`
   * that is based on the `G` instance of the base type.
   *
   * Typical examples for `G` are decoders.
   */
  implicit def refTypeViaMonadError[F[_, _], G[_], T, P](implicit
      m: MonadError[G, String],
      rt: RefType[F],
      v: Validate[T, P],
      gt: G[T]
  ): G[F[T, P]] =
    m.flatMap(gt)(t => m.fromEither(rt.refine[P](t)))

  /**
   * `Validate` instance for `F` via `Foldable[F]`.
   *
   * Examples: NonEmptyList, NonEmptyVector.
   */
  implicit def sizeValidateViaFoldable[F[_]: Foldable, T, P, RP](
      implicit
      v: Validate.Aux[Int, P, RP]): Validate.Aux[F[T], Size[P], Size[v.Res]] =
    new Validate[F[T], Size[P]] {
      override type R = Size[v.Res]

      override def validate(t: F[T]): Res = {
        val r = v.validate(size(t))
        r.as(Size(r))
      }

      override def showExpr(t: F[T]): String =
        v.showExpr(size(t))

      override def showResult(t: F[T], r: Res): String = {
        val s = size(t)
        val nested = v.showResult(s, r.detail.p)
        Resources.predicateTakingResultDetail(s"size($t) = $s", r, nested)
      }

      private[this] def size(t: F[T]): Int = {
        t.foldMap(_ => 1)
      }
    }
}
