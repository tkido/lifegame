package com.tkido.math

case class Vector(x:Int, y:Int) extends Ordered[Vector]{
  def unary_+ :Vector = Vector(x, y)
  def unary_- :Vector = Vector(-x, -y)
  
  def +(that:Vector) :Vector =
    Vector(x + that.x, y + that.y)
  def -(that:Vector) :Vector =
    Vector(x - that.x, y - that.y)

  def *(that: Vector) :Vector =
    Vector(x * that.x - y * that.y,
           x * that.y + y * that.x)
  def *(scalar:Int) :Vector =
    Vector(x * scalar, y * scalar)
  def /(scalar:Int) :Vector =
    Vector(x / scalar, y / scalar)
  
  def compare(that:Vector) =
    (y - that.y) match{
      case 0 => x - that.x
      case _ => y - that.y
    }
  
  override def toString = "(%d,%d)".format(x, y)
}