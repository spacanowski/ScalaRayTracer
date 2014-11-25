package pl.tracer.view

import scala.swing._
import java.awt.event.ActionListener
import pl.tracer.model.{ Pixel, Color }
import pl.tracer.app.RayTracerApp

object ScalaSwingRayTracer extends SimpleSwingApplication {
  val width: Int = 1200
  val height: Int = 750
  val antyaliasing: Boolean = false

  def top = new MainFrame {
    title = "Simple Scala Ray Tracer"
    contents = new CustomFrame(width, height, antyaliasing)
    size = new Dimension(if (antyaliasing) width / 2 else width, if (antyaliasing) height / 2 else height)
  }
}
class CustomFrame(width: Int, height: Int, antyaliasing: Boolean) extends Panel {
  val pixels: Seq[Pixel] = pixelList
  override val size: Dimension = new Dimension(fixDimensionValue(width), fixDimensionValue(height))
  override val ignoreRepaint: Boolean = true

  def fixDimensionValue(value: Int) = if (antyaliasing) value / 2 else value
  def pixelList = if (antyaliasing) {
    val initialPixels = new RayTracerApp(width, height).makeView
    var anty = Seq[Pixel]()
    for {
      y <- 0 until width - 2 by 2
      x <- 0 until height - 2 by 2
    } {
      var newR: Double = 0
      var newG: Double = 0
      var newB: Double = 0
      for {
        nx <- 0 to 1
        ny <- 0 to 1
      } {
        val color = initialPixels((x + nx) * height + y + ny).color
        newR += color.r
        newG += color.g
        newB += color.b
      }
      anty = anty :+ (Pixel(x / 2, y / 2, Color(newR / 4, newG / 4, newB / 4)))
    }
    anty
  } else
    new RayTracerApp(width, height).makeView

  override def paint(g: Graphics2D) = {
    paintPixels(pixels, g)
  }

  def paintPixels(pixels: Seq[Pixel], g: Graphics2D) =
    pixels.foreach { p =>
      {
        g.setColor(new java.awt.Color(p.color.r.toFloat, p.color.g.toFloat, p.color.b.toFloat))
        g.drawRect(p.x, p.y, 1, 1)
      }
    }
}
