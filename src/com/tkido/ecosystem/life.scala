package com.tkido.ecosystem

import java.awt.{Dimension, Graphics2D, Graphics, Image, Point, Rectangle}
import java.awt.Color
import java.awt.geom.Ellipse2D

import com.tkido.quadtree
import com.tkido.math.Vector

abstract class Life() extends quadtree.Mover{
  val field:Field
  var v:Vector
  var dv:Vector
  var radius:Double
  var energy:Double
  
  val color:Color
  
  def paint(g: Graphics2D) {
    g.setColor(color)
    g.fill(new Ellipse2D.Double(v.x - radius, v.y - radius, radius * 2 , radius * 2))
  }
  
  def update{
    radius = math.sqrt(energy)
    
    v += dv
    
    var x = v.x
    var y = v.y
    var dx = dv.x
    var dy = dv.y
    
    if(x - radius < 0.0){
      x = 2 * radius - x
      dx *= -1
    }else if(x + radius >= field.length){
      x = 2 * field.length -2 * radius - x
      dx *= -1
    }
    
    y += dy
    if(y - radius < 0.0){
      y = 2 * radius - y
      dy *= -1
    }else if(y + radius >= field.length){
      y = 2 * field.length -2 * radius - y
      dy *= -1
    }
    
    v = Vector(x, y)
    dv = Vector(dx, dy)
    
    updateCell()
  }
  
  def updateCell(){
    val x1 = field.sanitize(v.x - radius)
    val y1 = field.sanitize(v.y - radius)
    val x2 = field.sanitize(v.x + radius)
    val y2 = field.sanitize(v.y + radius)
    super.updateCell(x1, y1, x2, y2)
  }
  
  def check(other:quadtree.Mover)
  
}
