package image4s.laws

import image4s.Axis
import image4s.AxisCoordinate
import image4s.AxisCoordinatesRecord
import image4s.AxisUnit
import image4s.NonSpatialAxes

/** Reusable laws for ordered non-spatial sampling coordinates. */
object AxisLaws:
  def coordinateCountMatchesExtent(axis: Axis): Boolean =
    axis.extent > 0 &&
      Vector.tabulate(axis.extent)(axis.coordinateAt).forall(_.isRight) &&
      axis.coordinateAt(-1).isLeft &&
      axis.coordinateAt(axis.extent).isLeft

  def coordinateLookupMatchesRecord(axis: Axis): Boolean =
    axis.record.coordinates match
      case AxisCoordinatesRecord.Ordinal(extent) =>
        Vector.tabulate(extent)(index =>
          axis.coordinateAt(index).contains(AxisCoordinate.Ordinal(index))
        ).forall(identity)
      case AxisCoordinatesRecord.Regular(
            extent,
            origin,
            step,
            unitId
          ) =>
        AxisUnit.fromId(unitId).exists { unit =>
          Vector.tabulate(extent)(index =>
            axis.coordinateAt(index).contains(
              AxisCoordinate.Numeric(
                origin + step * index.toDouble,
                unit
              )
            )
          ).forall(identity)
        }
      case AxisCoordinatesRecord.Explicit(values, unitId) =>
        AxisUnit.fromId(unitId).exists { unit =>
          values.zipWithIndex.forall { case (value, index) =>
            axis
              .coordinateAt(index)
              .contains(AxisCoordinate.Numeric(value, unit))
          }
        }
      case AxisCoordinatesRecord.Categorical(labels) =>
        labels.zipWithIndex.forall { case (label, index) =>
          axis
            .coordinateAt(index)
            .contains(AxisCoordinate.Categorical(label))
        }

  def recordRoundTrip(axis: Axis): Boolean =
    Axis.fromRecord(axis.record).exists(_.record == axis.record)

  def permutationRoundTrip(
      axes: NonSpatialAxes,
      order: Vector[Int]
  ): Boolean =
    if order.size != axes.size ||
      order.sorted != axes.values.indices.toVector
    then false
    else
      val inverse =
        Vector.tabulate(order.size)(source => order.indexOf(source))
      axes
        .permute(order)
        .flatMap(_.permute(inverse))
        .exists(_.records == axes.records)
