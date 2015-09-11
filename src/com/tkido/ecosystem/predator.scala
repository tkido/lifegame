package com.tkido.ecosystem

import java.awt.Color
import scala.util.Random
import com.tkido.math.Vector
import com.tkido.quadtree.Mover

object Predator {
  val speed = 2.0
  val range = 20.0
  val color:Color = new Color(255, 0, 0)
}

class Predator(val field:Field, var v:Vector) extends Life{
  var radius:Double = 1.0
  var energy:Double = 1.0
  var dv = Vector(0.0, 0.0)
  
  var seek = false
  var target:Option[Vector] = None

  val color:Color = Predator.color
  
  def check(other:Mover){
    if(this.equals(other))
      return
    other match{
      case g:Grazer =>
        if((v - g.v).size < radius + g.radius){
          energy += g.energy / 10
          g.energy = 0.0
        }
        if((v - g.v).size < radius + Predator.range + g.radius){
          target = target match {
            case Some(w) =>
              if((v - g.v).size < (v - w).size)
                Some(g.v)
              else
                target
            case None =>
              Some(g.v)
          }
        }
      case _ =>
    }
  }
  
  override def update{
    target match{
      case Some(w) =>
        seek = false
        dv = (w - v).regulated * Predator.speed
      case None =>
        if(!seek){
          seek = true
          dv = com.tkido.math.nextVector * Predator.speed
        }
    }
    target = None
    
    super.update
    
    if(energy >= 11.0){
      energy = 10.0
      Range(0, 1).map(_ => field.addLife(new Predator(field, v)))
    }
    
    energy *= 0.999
    energy -= 0.0001
  }
  
  override def updateCell(){
    val x1 = field.sanitize(v.x - radius - Predator.range)
    val y1 = field.sanitize(v.y - radius - Predator.range)
    val x2 = field.sanitize(v.x + radius + Predator.range)
    val y2 = field.sanitize(v.y + radius + Predator.range)
    super.updateCell(x1, y1, x2, y2)
  }
}