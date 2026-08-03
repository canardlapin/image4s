package image4s.geometry

import java.util.concurrent.Executors

import munit.FunSuite

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.*

final class RegistryConcurrencySuite extends FunSuite:
  test("immutable frame and grid registries restore concurrently"):
    val frameId = geometryRight(FrameId.parse("concurrent-frame"))
    val gridId = geometryRight(GridId.parse("concurrent-grid"))
    val original =
      geometryRight(
        Frame.persistentNamed[D3](
          frameId,
          "concurrent owner",
          unit = LengthUnit.Millimeter,
          convention = CoordinateConvention.RAS
        )
      )
    val frameRecord = geometryRight(original.record)
    val firstFrame =
      geometryRight(
        Frame.restore[D3](frameRecord, Frame.Registry.empty)
      )
    val originalGrid =
      geometryRight(
        Grid.createPersistent(gridId, firstFrame.frame)(
          Vector(8, 9, 10),
          Affine.identity[D3]
        )
      )
    val gridRecord = geometryRight(originalGrid.record)
    val firstGrid =
      geometryRight(
        Grid.restore(
          gridRecord,
          firstFrame.frame,
          Grid.Registry.empty
        )
      )
    val executor = Executors.newFixedThreadPool(8)
    given ExecutionContext =
      ExecutionContext.fromExecutorService(executor)

    try
      val restorations =
        Vector.fill(256) {
          Future {
            val frame =
              geometryRight(
                Frame.restore[D3](
                  frameRecord,
                  firstFrame.registry
                )
              )
            val grid =
              geometryRight(
                Grid.restore(
                  gridRecord,
                  frame.frame,
                  firstGrid.registry
                )
              )
            (frame, grid)
          }
        }
      val completed =
        Await.result(Future.sequence(restorations), 20.seconds)

      completed.foreach { case (frame, grid) =>
        assert(frame.frame.sameRuntimeOwnerAs(firstFrame.frame))
        assert(frame.registry eq firstFrame.registry)
        assert(grid.grid.sameRuntimeOwnerAs(firstGrid.grid))
        assert(grid.registry eq firstGrid.registry)
      }
      assertEquals(firstFrame.registry.size, 1)
      assertEquals(firstGrid.registry.size, 1)
    finally executor.shutdownNow(): Unit

  test("concurrent conflicting restores fail deterministically without mutation"):
    val id = geometryRight(FrameId.parse("concurrent-conflict"))
    val baseline =
      geometryRight(
        Frame.persistentNamed[D2](
          id,
          "baseline",
          unit = LengthUnit.Millimeter,
          convention = CoordinateConvention.RAS
        )
      )
    val conflict =
      geometryRight(
        Frame.persistentNamed[D2](
          id,
          "conflict",
          unit = LengthUnit.Meter,
          convention = CoordinateConvention.RAS
        )
      )
    val baselineRecord = geometryRight(baseline.record)
    val conflictRecord = geometryRight(conflict.record)
    val registry =
      geometryRight(
        Frame.restore[D2](
          baselineRecord,
          Frame.Registry.empty
        )
      ).registry
    val executor = Executors.newFixedThreadPool(8)
    given ExecutionContext =
      ExecutionContext.fromExecutorService(executor)

    try
      val conflicts =
        Vector.fill(256) {
          Future(Frame.restore[D2](conflictRecord, registry))
        }
      val completed =
        Await.result(Future.sequence(conflicts), 20.seconds)

      completed.foreach {
        case Left(_: GeometryError.FrameKeyConflict) => ()
        case other =>
          fail(s"expected a stable FrameKeyConflict, found $other")
      }
      assertEquals(registry.size, 1)
      val restored =
        geometryRight(Frame.restore[D2](baselineRecord, registry))
      assertEquals(restored.registry.size, 1)
    finally executor.shutdownNow(): Unit

  private def geometryRight[A](
      value: Either[GeometryError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
