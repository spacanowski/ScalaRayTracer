package pl.tracer.engine

import scala.math.{pow, tan, sqrt}
import pl.tracer.model._

object RayTracer {
  def makeVector(point: Point, point1: Point): Vector =
    Vector(point1.x - point.x, point1.y - point.y, point1.z - point.z)
}
class RayTracer(camera: Camera, sphereList: Seq[Sphere], lightList: Seq[Light], recurentionDegree: Int) {
  val shadowReduceFactor: Double = 0.7
  val shadowColorReduce: Double = 0.3
  val specAlfa = 35
  val camerXPosition = camera.position.x
  val cameraYPosition = camera.position.y
  val cameraZPosition = camera.position.z

  def pow2(value: Double): Double = pow(value, 2)

  def by2(value: Double): Double = value / 2

  def calcullateDelta(b: Double, c: Double): Double =
    pow2(b) - 4 * c

  def calculateB(xd: Double, x0: Double, xc: Double, yd: Double, y0: Double, yc: Double, zd: Double, z0: Double, zc: Double): Double =
    2 * (xd * (x0 - xc) + yd * (y0 - yc) + zd * (z0 - zc))

  def calculateC(x0: Double, xc: Double, y0: Double, yc: Double, z0: Double, zc: Double, radius: Double): Double =
    pow2(x0 - xc) + pow2(y0 - yc) + pow2(z0 - zc) - pow2(radius)

  def calculateCutPoint(pos: Point, dir: Vector, dis: Double): Point =
    Point(pos.x + dir.x * dis, pos.y + dir.y * dis, pos.z + dir.z * dis)

  def calculateNormal(point: Point, sphere: Sphere): Vector =
    Vector((point.x - sphere.position.x) / sphere.radius, (point.y - sphere.position.y) / sphere.radius,
      (point.z - sphere.position.z) / sphere.radius) normalize

  def fixIfOutbands(x: Double): Double = if (x > 1) 1 else if (x < 0) 0 else x

  def calculateCutPoints(deltaSqrt: Double, b: Double): (Double, Double) = (by2(b + deltaSqrt), by2(b - deltaSqrt))

  def calculateSphereIndexes(delta: Double, b: Double, sphere: Sphere, oldT: Double, which: Int): (Double, Int) = {
    val (t1, t2) = calculateCutPoints(sqrt(delta), -b)
    if (t1 >= 0 || t2 >= 0) {
      val t = if (t1 < t2) t1 else t2
      if (t < oldT) (t, sphereList.indexOf(sphere))
      else (oldT, which)
    } else (oldT, which)
  }

  def calcuteSphereShadow(dirSphereLight: Vector, light: Light, sphereList: Seq[Sphere],
                          which: Int, red: Double, green: Double, blue: Double): (Double, Double, Double) =
    if (sphereList.isEmpty) (red, green, blue) else if (sphereList.indexOf(sphereList.head) == which)
      calcuteSphereShadow(dirSphereLight, light, sphereList.tail, which, red, green, blue)
    else {
      val sphereIn: Sphere = sphereList.head
      val delta: Double = calcullateDelta(calculateB(dirSphereLight.x, sphereIn.position.x, light.position.x,
        dirSphereLight.y, sphereIn.position.y, light.position.y, dirSphereLight.z, sphereIn.position.z, light.position.z),
        calculateC(sphereIn.position.x, light.position.x, sphereIn.position.y, light.position.y, sphereIn.position.z,
          light.position.z, sphereIn.radius))

      if (delta >= 0) (red - shadowColorReduce, green - shadowColorReduce, blue - shadowColorReduce)
      else (red, green, blue)
    }

  def reduceReflectedColor(color: Double, recValue: Int): Double = color * (shadowReduceFactor * recValue)

  def calculatePixelColor(color: Double, lightColor: Double, sphereColor: Double,
                          specular: Double, dotProduct: Double): Double = color + lightColor * (sphereColor + specular) * dotProduct

  def calculateColorsFromEfects(currentSphereIndex: Int, sphereColor: Color, light: Light, cutPoint: Point, normalInPoint: Vector,
                                red: Double, green: Double, blue: Double): (Double, Double, Double) = {
    val dotProduct: Double = normalInPoint dotProduct (RayTracer.makeVector(cutPoint, light.position) normalize)
    val specular: Double = if (dotProduct > 0) pow(dotProduct, specAlfa) else 0
    calcuteSphereShadow(RayTracer.makeVector(light.position, cutPoint) normalize, light,
      sphereList, currentSphereIndex, calculatePixelColor(red, light.color.r, sphereColor.r, specular, dotProduct),
      calculatePixelColor(green, light.color.g, sphereColor.g, specular, dotProduct),
      calculatePixelColor(blue, light.color.b, sphereColor.b, specular, dotProduct))
  }

  def calculateLightColorComponent(lights: Seq[Light], currentSphereIndex: Int, sphereColor: Color,
                                   cutPoint: Point, normalInPoint: Vector, r: Double, g: Double, b: Double): (Double, Double, Double) =
    if (lights.isEmpty) (r, g, b) else {
      val (newRed, newGreen, newBlue) =
        calculateColorsFromEfects(currentSphereIndex, sphereColor, lights.head, cutPoint, normalInPoint, r, g, b)
      calculateLightColorComponent(lights.tail, currentSphereIndex, sphereColor, cutPoint, normalInPoint, newRed, newGreen, newBlue)
    }

  def calculateShadowColorComponent(ray: Ray, recValue: Int, sphereColor: Color, normalInPoint: Vector,
                                    shouldReflectRay: Boolean): (Double, Double, Double) =
    if (recValue < recurentionDegree && shouldReflectRay) {
      val nextRecValue = recValue + 1
      val colorFromReflection: Color = core(Ray(ray.vector reflected (normalInPoint), Color(0, 0, 0)),
        nextRecValue, 0, 0, 0)
      (reduceReflectedColor(colorFromReflection.r, nextRecValue),
        reduceReflectedColor(colorFromReflection.g, nextRecValue),
        reduceReflectedColor(colorFromReflection.b, nextRecValue))
    } else (0, 0, 0)

  def findCrossingSphere(ray: Ray, spheres: Seq[Sphere], distance: Double, index: Int): (Double, Int) =
    if (spheres.isEmpty) (distance, index)
    else {
      val sphere = spheres.head
      val b: Double = calculateB(ray.vector.x, camerXPosition, sphere.position.x,
        ray.vector.y, cameraYPosition, sphere.position.y, ray.vector.z, cameraZPosition, sphere.position.z)
      val delta: Double = calcullateDelta(b, calculateC(camerXPosition, sphere.position.x,
        cameraYPosition, sphere.position.y, cameraZPosition, sphere.position.z, sphere.radius))

      if (delta >= 0) {
        val (oldDistance, currentSphereIndex) = calculateSphereIndexes(delta, b, sphere, distance, index)
        findCrossingSphere(ray, spheres.tail, oldDistance, currentSphereIndex)
      } else findCrossingSphere(ray, spheres.tail, distance, index)
    }

  def core(ray: Ray, recValue: Int, red: Double, green: Double, blue: Double): Color = {
    val (oldDistance, currentSphereIndex) = findCrossingSphere(ray, sphereList, Double.MaxValue, -1)

    if (currentSphereIndex >= 0) {
      val cutPoint: Point = calculateCutPoint(camera.position, ray.vector, oldDistance)
      val sphere: Sphere = sphereList(currentSphereIndex);
      val normalInPoint: Vector = calculateNormal(cutPoint, sphere)

      val (r, g, b) =
        calculateShadowColorComponent(ray, recValue, sphere.color, normalInPoint, sphere.reflection)
      val (iterationRed, iterationGreen, iterationBlue) =
        calculateLightColorComponent(lightList, currentSphereIndex, sphere.color, cutPoint, normalInPoint, r, g, b)
      Color(fixIfOutbands(iterationRed), fixIfOutbands(iterationGreen), fixIfOutbands(iterationBlue))
    } else
      Color(fixIfOutbands(red), fixIfOutbands(green), fixIfOutbands(blue))
  }
}
