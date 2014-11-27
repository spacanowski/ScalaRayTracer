package pl.tracer.app

import scala.math.{Pi, tan}

import pl.tracer.engine._
import pl.tracer.model._

class RayTracerApp(width: Int, height: Int) {
  private val recurentionDegree = 4;
  private val camera = Camera(Point(0, 0, 100), Point(0, 0, 0), Vector(0, 1, 0), 60 * (Pi / 180))
  private val sphereList = Seq(Sphere(10, Vector(10, 0, 0), Color(0, 1, 0), false, true),
    Sphere(10, Vector(20, 0, 75), Color(1, 0.33, 1), false, true),
    Sphere(5, Vector(-10, 0, 90), Color(0.5, 0, 0.5), false, true),
    Sphere(5, Vector(8, 15, 10), Color(0, 0, 1), false, true),
    Sphere(2, Vector(10, 0, 15), Color(1, 0, 0), false, true))
  private val lightList = Seq(Light(Point(30, 30, 160), Color(1, 1, 1)),
    Light(Point(-30, 30, 60), Color(1, 1, 1)))
  private val lookAt = RayTracer.makeVector(camera.position, camera.lookAt)
  private val crosspro = lookAt crossProduct (camera.up)
  private val u = crosspro normalize
  private val v = lookAt vCalculation (u)
  private val o = oCalculation(width, height, u, v, camera.fov, lookAt normalize)
  private val matrix = Matrix(u, v, o)
  private val rayTracer = new RayTracer(camera, sphereList, lightList, recurentionDegree)

  private def oCalculation(width: Int, height: Int, u: Vector, v: Vector, fov: Double, normLookAt: Vector): Vector =
    normLookAt productVectorNumber (width / (2 * (tan(by2(fov))))) subtract (u productVectorNumber by2(width)) subtract (v productVectorNumber (by2(height)))

  private def by2(value: Double): Double = value / 2

  private def by2(value: Int): Double = value / 2

  def makeView(): Seq[Pixel] = (for {
    i <- 0 to width
    j <- 0 to height
  } yield Pixel(i, j, rayTracer.core(Ray((matrix productMV (Vector(i, j, 1)) normalize), new Color(0, 0, 0)), 0, 1, 1, 1)))
}
