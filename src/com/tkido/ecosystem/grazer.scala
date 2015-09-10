package com.tkido.ecosystem

import java.awt.Color
import scala.util.Random
import com.tkido.math.Vector
import com.tkido.quadtree.Mover

object Grazer {
  val speed = 1.0
  val range = 10.0
  val color:Color = new Color(0, 0, 255)
}


class Grazer(var v:Vector) extends Life{
  var radius:Double = 1.0
  var energy:Double = 1.0
  var dv = Vector(0.0, 0.0)
  
  var seek = false
  var target:Option[Vector] = None
  var enemy:Option[Vector] = None

  val color:Color = Grazer.color
  
  def check(other:Mover){
    if(this.equals(other))
      return
    other match{
      case p:Plant =>
        if((v - p.v).size < radius + p.radius){
          energy += p.energy / 10
          p.energy = 0.0
        }
        if((v - p.v).size < radius + Grazer.range + p.radius){
          target = target match {
            case Some(w) =>
              if((v - p.v).size < (v - w).size)
                Some(p.v)
              else
                target
            case None =>
              Some(p.v)
          }
        }
      case p:Predator =>
        if((v - p.v).size < radius + Grazer.range + p.radius){
          enemy = enemy match {
            case Some(w) =>
              if((v - p.v).size < (v - w).size)
                Some(p.v)
              else
                enemy
            case None =>
              Some(p.v)
          }
        }
      case _ =>
    }
  }
  
  override def update{
    enemy match {
      case Some(w) =>
        seek = false
        dv = (v - w).regulated * Grazer.speed
      case None =>
        target match{
          case Some(w) =>
            seek = false
            dv = (w - v).regulated * Grazer.speed
          case None =>
            if(!seek){
              seek = true
              dv = com.tkido.math.nextVector * Grazer.speed
            }
        }
    }
    enemy = None
    target = None
    
    super.update
    
    if(energy >= 12.0){
      energy = 10.0
      Range(0, 2).map(_ => main.addLife(new Grazer(v)))
    }
    
    energy *= 0.995
    energy -= 0.005
  }
  
  override def updateCell(){
    val x1 = sanitize(v.x - radius - 10)
    val y1 = sanitize(v.y - radius - 10)
    val x2 = sanitize(v.x + radius + 10)
    val y2 = sanitize(v.y + radius + 10)
    super.updateCell(x1, y1, x2, y2)
  }
}