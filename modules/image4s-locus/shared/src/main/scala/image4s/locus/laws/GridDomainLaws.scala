package image4s.locus.laws

import image4s.locus.GridDomain
import image4s.locus.GridDomainView
import image4s.locus.SpatialFieldView
import locus4s.Region
import locus4s.Selection
import locus4s.data.Field

/** Reusable, representation-neutral laws for the image4s-locus bridge. */
object GridDomainLaws:
  def ordinalIndexRoundTrip[
      F <: image4s.geometry.Frame[D],
      D <: image4s.geometry.Dim,
      S
  ](
      bridge: GridDomain[F, D, S],
      ordinal: Int
  )(using image4s.geometry.Dimension[D]): Boolean =
    bridge
      .indexOfOrdinal(ordinal)
      .flatMap(bridge.ordinalOf)
      .contains(ordinal)

  def exactViewCommutesWithField[
      D <: image4s.geometry.Dim,
      S,
      A
  ](
      source: Field[S, A],
      view: GridDomainView[D, S]
  )(
      target: Field[view.T, A]
  ): Boolean =
    target.space.sameRuntimeOwnerAs(view.target) &&
      view.target.indices.forall: index =>
        target(index) == source(view.injection(index))

  def bijectiveRegionPreservesCardinality[D <: image4s.geometry.Dim, S](
      region: Region[S],
      view: GridDomainView[D, S]
  ): Boolean =
    view.bijection.forall: bijection =>
      bijection.toTotalMap.pullback(region).cardinality ==
        region.cardinality

  def selectionGatherFollowsOrder[S, A](
      field: Field[S, A],
      selection: Selection[S]
  ): Boolean =
    field.gather(selection).toVector ==
      selection.indices.map(field.apply).toVector

  def sharesImageStorage[
      S,
      I <: image4s.SampleSpace[?, ?],
      A,
      Sem,
      R <: ravel.AnyRank
  ](
      field: SpatialFieldView[S, I, A, Sem, R]
  ): Boolean =
    field.sourceData eq field.image.data
