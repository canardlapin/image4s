package image4s.locus

import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit
import locus4s.DomainRegistry
import munit.FunSuite

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.*

final class GridDomainConcurrencySuite extends FunSuite:
  test("concurrent restores through one immutable registry converge safely"):
    val frame =
      right(
        Frame.persistentNamed[D3](
          right(FrameId.parse("frame-concurrent")),
          "concurrent",
          LengthUnit.Millimeter,
          CoordinateConvention.RAS
        )
      )
    val grid =
      right(
        Grid.createPersistent(
          right(GridId.parse("grid-concurrent")),
          frame
        )(
          Vector(8, 9, 10),
          Affine.identity[D3]
        )
      )
    val seeded =
      right(GridDomain.register(grid, "voxels", DomainRegistry.empty))
    val restored =
      Await.result(
        Future.traverse(0 until 256): _ =>
          Future(
            right(
              GridDomain.restore(
                seeded.value.record,
                grid,
                seeded.registry
              )
            )
          ),
        30.seconds
      )

    assertEquals(restored.size, 256)
    assert(
      restored.forall(_.value.space.sameRuntimeOwnerAs(seeded.value.space))
    )
    assert(restored.forall(_.registry.size == 1))
    assert(restored.forall(_.value.space.key == seeded.value.space.key))

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
