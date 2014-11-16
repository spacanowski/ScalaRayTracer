package pl.tracer.utils

import scala.io.Source
import pl.tracer.model._

/**
 * Not finished. Will be reading scene (tracer data) from file
 */
class SceneLoader {

  def loadScene(filePath: String) = {
    val source = Source.fromFile(filePath)
    val config = for (l <- source.getLines)
      yield l match {
      case i if i.startsWith("L:")    => { val values = l.stripPrefix("L:").split("\\s").map(_.toDouble); Light(Point(values(0), values(1), values(2)), Color(values(3), values(4), values(5))) }
      case i if i.startsWith("S:")    => l.stripPrefix("S:").split("\\s").map(_.toDouble)
      case i if i.startsWith("C:")    => { val values = l.stripPrefix("C:").split("\\s").map(_.toDouble); Camera(Point(values(0), values(1), values(2)), Point(values(3), values(4), values(5)), Vector(values(6), values(7), values(8)), values(9)) }
      case i if i.startsWith("size:") => l.stripPrefix("size:").split("x").map(_.toInt)
      case _                          =>
    }
  }

}