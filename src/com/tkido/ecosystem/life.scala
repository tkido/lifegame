package com.tkido.ecosystem

import java.awt.{Dimension, Graphics2D, Graphics, Image, Point, Rectangle}
import java.awt.Color
import java.awt.geom.Ellipse2D

import com.tkido.quadtree

abstract class Life() extends quadtree.Mover{
  var x:Double
  var y:Double
  var dx:Double
  var dy:Double
  var radius:Double
  var energy:Double
  
  val color:Color
  
  protected def square(x:Double) :Double = x * x
  protected def sanitize(x:Double) :Double = {
    x match {
      case x if x < 0.0 => 0.0
      case x if x >= fieldLength => - 0.0001
      case _ => x
    }
  }
  
  def paint(g: Graphics2D) {
    g.setColor(color)
    g.fill(new Ellipse2D.Double(x - radius, y - radius, radius * 2 , radius * 2))
  }
  
  def update{
    radius = math.sqrt(energy)
    
    x += dx
    if(x - radius < 0.0){
      x = 2 * radius - x
      dx *= -1
    }else if(x + radius >= fieldLength){
      x = 2 * fieldLength -2 * radius - x
      dx *= -1
    }
    
    y += dy
    if(y - radius < 0.0){
      y = 2 * radius - y
      dy *= -1
    }else if(y + radius >= fieldLength){
      y = 2 * fieldLength -2 * radius - y
      dy *= -1
    }
    
    updateCell()
  }
  
  def updateCell(){
    val x1 = sanitize(x - radius)
    val y1 = sanitize(y - radius)
    val x2 = sanitize(x + radius)
    val y2 = sanitize(y + radius)
    super.updateCell(x1, y1, x2, y2)
  }
  
  def check(other:quadtree.Mover)
  
}
