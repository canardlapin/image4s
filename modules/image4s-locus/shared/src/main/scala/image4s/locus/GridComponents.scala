package image4s.locus

import image4s.geometry.D3
import image4s.geometry.Frame
import locus4s.Region

/** Adjacency in lattice coordinates, independent of world-space voxel spacing. */
enum GridConnectivity:
  case Face, FaceEdge, FaceEdgeCorner

object GridComponents:
  /** Partition active voxels using 6-, 18-, or 26-neighbor connectivity.
    *
    * The shared owner type requires the domain's exact live owner. Members are in domain order and
    * components are ordered by their smallest member ordinal. Empty support yields no components.
    * Storage is proportional to active support, rather than the full grid.
    */
  def connected[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      active: Region[S],
      connectivity: GridConnectivity = GridConnectivity.Face
  ): Vector[Region[S]] =
    val seeds = active.ordinalsInDomainOrder
    val remaining = scala.collection.mutable.HashSet.from(seeds)
    val queue = new Array[Int](seeds.length)
    val offsets = connectivity match
      case GridConnectivity.Face => faceOffsets
      case GridConnectivity.FaceEdge => faceEdgeOffsets
      case GridConnectivity.FaceEdgeCorner => allOffsets
    val shape = domain.grid.shape
    val components = Vector.newBuilder[Region[S]]

    var seedIndex = 0
    while seedIndex < seeds.length do
      val seed = seeds(seedIndex)
      if remaining.remove(seed) then
        queue(0) = seed
        var head = 0
        var tail = 1
        while head < tail do
          val coordinates = domain.coordinatesOfOrdinalUnchecked(queue(head))
          var offsetIndex = 0
          while offsetIndex < offsets.length do
            val (dx, dy, dz) = offsets(offsetIndex)
            val x = coordinates(0) + dx
            val y = coordinates(1) + dy
            val z = coordinates(2) + dz
            if x >= 0 && x < shape(0) && y >= 0 && y < shape(1) && z >= 0 && z < shape(2)
            then
              val neighbor = domain.ordinalOfCoordinatesUnchecked(Vector(x, y, z))
              if remaining.remove(neighbor) then
                queue(tail) = neighbor
                tail += 1
            offsetIndex += 1
          head += 1
        components += Region.fromIndices(
          domain.space,
          queue.iterator.take(tail).map(domain.space.indexAtValidatedOrdinal)
        )
      seedIndex += 1
    components.result()

  private val allOffsets =
    (for
      dx <- -1 to 1
      dy <- -1 to 1
      dz <- -1 to 1
      if dx != 0 || dy != 0 || dz != 0
    yield (dx, dy, dz)).toVector

  private val faceOffsets = allOffsets.filter: (dx, dy, dz) =>
    math.abs(dx) + math.abs(dy) + math.abs(dz) == 1

  private val faceEdgeOffsets = allOffsets.filter: (dx, dy, dz) =>
    math.abs(dx) + math.abs(dy) + math.abs(dz) <= 2
