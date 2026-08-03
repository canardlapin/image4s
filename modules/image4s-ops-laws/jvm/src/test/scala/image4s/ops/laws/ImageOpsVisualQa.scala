package image4s.ops.laws

import _root_.intaglio.RasterImage
import _root_.intaglio.alpha
import _root_.intaglio.blue
import _root_.intaglio.green
import _root_.intaglio.red
import java.awt.image.BufferedImage
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import javax.imageio.ImageIO

/** JVM-only visual-review artifact generator for image-operation stages.
  *
  * Run with `sbt imageOpsVisualQaJVM`; the output directory contains nearest-neighbour
  * presentations of the exact rasters used by [[ImageOpsVisualQaSuite]], plus a small HTML index.
  */
object ImageOpsVisualQa:
  private val PixelScale = 24

  def main(args: Array[String]): Unit =
    val root =
      Paths.get(args.headOption.getOrElse("target/image-ops-visual-qa"))
    Files.createDirectories(root)
    val cases = ImageOpsVisualQaFixtures.build()
    cases.foreach { example =>
      writePng(example.raster, root.resolve(s"${example.name}.png"))
    }
    Files.writeString(
      root.resolve("manifest.tsv"),
      cases
        .map(example => s"${example.name}\t${example.name}.png")
        .mkString(
          "case\tfile\n",
          "\n",
          "\n"
        ),
      StandardCharsets.UTF_8
    )
    Files.writeString(
      root.resolve("index.html"),
      html(cases.map(_.name)),
      StandardCharsets.UTF_8
    )
    println(s"wrote image-operation visual QA to $root")

  private def writePng(raster: RasterImage, path: Path): Unit =
    val image =
      new BufferedImage(
        raster.width * PixelScale,
        raster.height * PixelScale,
        BufferedImage.TYPE_INT_ARGB
      )
    var y = 0
    while y < raster.height do
      var x = 0
      while x < raster.width do
        val pixel = raster.pixelUnsafe(x, y)
        val argb =
          (pixel.alpha << 24) |
            (pixel.red << 16) |
            (pixel.green << 8) |
            pixel.blue
        var scaledY = 0
        while scaledY < PixelScale do
          var scaledX = 0
          while scaledX < PixelScale do
            image.setRGB(
              x * PixelScale + scaledX,
              y * PixelScale + scaledY,
              argb
            )
            scaledX += 1
          scaledY += 1
        x += 1
      y += 1
    if !ImageIO.write(image, "png", path.toFile) then
      throw new IllegalStateException(s"PNG writer unavailable for $path")

  private def html(names: Vector[String]): String =
    val sections = names
      .map { name =>
        s"""      <figure>
         |        <figcaption>$name</figcaption>
         |        <img src="$name.png" alt="$name image-operation stage">
         |      </figure>""".stripMargin
      }
      .mkString("\n")
    s"""<!doctype html>
       |<html lang="en">
       |  <head>
       |    <meta charset="utf-8">
       |    <meta name="viewport" content="width=device-width, initial-scale=1">
       |    <title>image4s operation visual QA</title>
       |    <style>
       |      body { margin: 0; padding: 2rem; color: #18212b; background: #f3f5f7; font: 15px/1.4 system-ui, sans-serif; }
       |      h1 { margin-top: 0; }
       |      main { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 1rem; }
       |      figure { margin: 0; padding: 1rem; background: white; border: 1px solid #d7dde3; border-radius: 8px; }
       |      figcaption { margin-bottom: 0.5rem; font-weight: 650; }
       |      img { display: block; width: 100%; height: auto; image-rendering: pixelated; border: 1px solid #edf0f2; }
       |    </style>
       |  </head>
       |  <body>
       |    <h1>image4s operation visual QA</h1>
       |    <p>Human-review gallery for the shared filter and morphology fixture.</p>
       |    <main>
       |$sections
       |    </main>
       |  </body>
       |</html>
       |""".stripMargin
