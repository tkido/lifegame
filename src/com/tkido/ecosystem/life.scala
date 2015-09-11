package com.tkido.ecosystem

import java.awt.{Dimension, Graphics2D, Image}
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
    field.move(this,
               v.x - radius,
               v.y - radius,
               v.x + radius,
               v.y + radius)
  }
  
  def check(other:quadtree.Mover)
  
}
