package image4s.geometry

import java.util.concurrent.Callable
import java.util.concurrent.Executors

import scala.jdk.CollectionConverters.*

final class GeometryRegistryJvmSuite extends munit.FunSuite:
  test("immutable registries support 1000 deterministic concurrent restores"):
    val id = right(FrameId.parse("frame-concurrent"))
    val source =
      right(
        Frame.persistentNamed[D3](
          id,
          "concurrent",
          LengthUnit.Millimeter,
          CoordinateConvention.RAS
        )
      )
    val record = right(source.record)
    val base = FrameRegistry.empty
    val executor = Executors.newFixedThreadPool(8)
    try
      val tasks =
        Vector.fill(1000)(
          new Callable[(FrameKey, Int)]:
            def call(): (FrameKey, Int) =
              val resolution =
                right(Frame.restore[D3](record, base))
              (
                resolution.frame.persistentKey.getOrElse(
                  fail("restored frame lost its persistent key")
                ),
                resolution.registry.size
              )
        )
      val results =
        executor
          .invokeAll(tasks.asJava)
          .asScala
          .map(_.get())
          .toVector

      assertEquals(base.size, 0)
      assertEquals(results.size, 1000)
      assert(results.forall(_ == (record.key, 1)))
    finally executor.shutdown()

  private def right[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
