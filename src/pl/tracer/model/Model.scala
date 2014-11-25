package pl.tracer.model

import scala.math.{ pow, sqrt }

case class Color(val r: Double, val g: Double, val b: Double)

case class Light(val position: Point, val color: Color)

case class Pixel(val x: Int, val y: Int, val color: Color)

case class Ray(val vector: Vector, val color: Color)

case class Camera(val position: Point, val lookAt: Point, val up: Vector, val fov: Double)

case class Sphere(val radius: Double, val position: Vector, val color: Color, val transparency: Boolean, val reflection: Boolean)

case class Point(val x: Double, val y: Double, val z: Double) {
  def crossProduct(vector: Vector): Vector =
    Vector(y * vector.z - z * vector.y, -(x * vector.z - z * vector.x), x * vector.y - y * vector.x)
}

case class Matrix(val v: Vector, val v1: Vector, val v2: Vector) {
  val matrix = Array(Vector(v.x, v1.x, v2.x), Vector(v.y, v1.y, v2.y), Vector(v.z, v1.z, v2.z))

  def productMV(vector: Vector): Vector = Vector(matrix(0) dotProduct (vector), matrix(1) dotProduct (vector), matrix(2) dotProduct (vector))
}

case class Vector(val x: Double, val y: Double, val z: Double) {
  def dotProduct(vector: Vector): Double = x * vector.x + y * vector.y + z * vector.z

  def crossProduct(vector: Vector): Vector =
    Vector(y * vector.z - z * vector.y, -(x * vector.z - z * vector.x), x * vector.y - y * vector.x)

  def negate: Vector = Vector(-x, -y, -z)

  def summerize(vector: Vector): Vector =
    Vector(x + vector.x, y + vector.y, z + vector.z)

  def productVectorNumber(number: Double): Vector =
    Vector(x * number, y * number, z * number)

  def subtract(vector: Vector): Vector =
    Vector(x - vector.x, y - vector.y, z - vector.z)

  def pow2(value: Double): Double = pow(value, 2)

  def magnitude: Double = sqrt(pow2(x) + pow2(y) + pow2(z))

  def normalize: Vector =
    Vector(x / magnitude, y / magnitude, z / magnitude)

  def reflected(normal: Vector): Vector =
    normal productVectorNumber (-(dotProduct(normal)) * 2) summerize normalize

  def vCalculation(u: Vector): Vector =
    crossProduct(u) normalize
}
