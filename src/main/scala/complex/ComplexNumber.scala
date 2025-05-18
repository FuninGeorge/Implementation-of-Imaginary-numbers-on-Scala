package complex

import math.{abs, atan2, hypot}
import scala.math.Numeric.Implicits.*

final case class ComplexNumber(real: Double, imaginary: Double) {
  def *(other: ComplexNumber) =
    ComplexNumber(
      (real * other.real) - (imaginary * other.imaginary),
      (real * other.imaginary) + (imaginary * other.real)
    )

  def +(other: ComplexNumber) =
    ComplexNumber(real + other.real, imaginary + other.imaginary)

  def ~=(o: ComplexNumber) =
    abs(real - o.real) < 1e-6 && abs(imaginary - o.imaginary) < 1e-6
}

given [T: Numeric]: Conversion[T, ComplexNumber] = num =>
  ComplexNumber(Numeric[T].toDouble(num), 0.0)

extension (cn: ComplexNumber)
  def -(other: ComplexNumber): ComplexNumber =
    ComplexNumber(cn.real - other.real, cn.imaginary - other.imaginary)

  def /(other: ComplexNumber): ComplexNumber = {
    val denominator = hypot(other.imaginary, other.real)
    ComplexNumber(
      (cn.real * other.real + cn.imaginary * other.imaginary) / denominator,
      (cn.imaginary * other.real - cn.real * other.imaginary) / denominator
    )
  }

  def toPolar: PolarForm = {
    val r = hypot(cn.real, cn.imaginary)
    val phi = atan2(cn.imaginary, cn.real)
    PolarForm(r, phi)
  }

case class PolarForm(r: Double, phi: Double)

extension [V: Numeric](num: V)
  def i: ComplexNumber = ComplexNumber(0, Numeric[V].toDouble(num))

object Main {
  def main(args: Array[String]): Unit = {
    val a = ComplexNumber(1, 1)
    val b: Long = 1
    val c = a + b
    val c2: ComplexNumber = 1 + a
    val c3 = a + 1

    val c34: ComplexNumber = 3 + 4.i
    println(c3) // ComplexNumber(3.0,4.0)

    // Тест операций
    println(a + 5) // ComplexNumber(6.0,1.0)
    println(5 + a) // ComplexNumber(6.0,1.0)
    println(a - 2) // ComplexNumber(-1.0,1.0)
    println(2 - a) // ComplexNumber(1.0,-1.0)
    println(a * 3) // ComplexNumber(3.0,3.0)
    println(a / 2) // ComplexNumber(0.5,0.5)
  }


}