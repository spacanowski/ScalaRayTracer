package pl.tracer.app

import scala.math.Pi
import scala.math.tan

import pl.tracer.engine._
import pl.tracer.model._

class RayTracerApp(width: Int, height: Int) {
  val recurentionDegree = 4;
  val camera = Camera(Point(0, 0, 100), Point(0, 0, 0), Vector(0, 1, 0), 60 * (Pi / 180))
  val sphereList = List(Sphere(10, Vector(10, 0, 0), Color(0, 1, 0), false, true),
    Sphere(10, Vector(20, 0, 75), Color(1, 0.33, 1), false, true),
    Sphere(5, Vector(-10, 0, 90), Color(0.5, 0, 0.5), false, true),
    Sphere(5, Vector(8, 15, 10), Color(0, 0, 1), false, true),
    Sphere(2, Vector(10, 0, 15), Color(1, 0, 0), false, true))
  val lightList = List(Light(Point(30, 30, 160), Color(1, 1, 1)),
    Light(Point(-30, 30, 60), Color(1, 1, 1)))
  val lookAt = RayTracer.makeVector(camera.position, camera.lookAt)
  val crosspro = lookAt crossProduct (camera.up)
  val u = crosspro normalize
  val v = lookAt vCalculation (u)
  val o = oCalculation(width, height, u, v, camera.fov, lookAt normalize)
  val matrix = Matrix(u, v, o)
  val rayTracer = new RayTracer(camera, sphereList, lightList, recurentionDegree)

  def oCalculation(width: Int, height: Int, u: Vector, v: Vector, fov: Double, normLookAt: Vector): Vector =
    normLookAt productVectorNumber (width / (2 * (tan(by2(fov))))) subtract (u productVectorNumber by2(width)) subtract (v productVectorNumber (by2(height)))

  def by2(value: Double): Double = value / 2

  def by2(value: Int): Double = value / 2

  def makeView(): Array[Pixel] = (for {
    i <- 0 to width
    j <- 0 to height
  } yield Pixel(i, j, rayTracer.core(Ray((matrix productMV (Vector(i, j, 1)) normalize), new Color(0, 0, 0)), 0, 1, 1, 1))) toArray
}