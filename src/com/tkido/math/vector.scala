package com.tkido.math

import scala.math.sqrt

case class Vector(x:Double, y:Double) extends Ordered[Vector]{
  def unary_+ = this
  def unary_- = Vector(-x, -y)
  
  def +(that:Vector) =
    Vector(x + that.x, y + that.y)
  def -(that:Vector) =
    Vector(x - that.x, y - that.y)

  def *(that: Vector) =
    Vector(x * that.x - y * that.y,
           x * that.y + y * that.x)
  def *(scalar:Double) =
    Vector(x * scalar, y * scalar)
  def /(scalar:Double) =
    Vector(x / scalar, y / scalar)
  
  def compare(that:Vector) =
    (y - that.y) match{
      case 0.0 =>
        (x - that.x) match{
          case 0.0 => 0
          case d if d > 0.0 => 1
          case _ => -1
        }
      case d if d > 0.0 => 1
      case _ => -1
    }
  
  def size = sqrt(x * x + y * y)
  def regulated = this / size
  
  override def toString = "(%s,%s)".format(x, y)
}