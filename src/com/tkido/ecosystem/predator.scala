package com.tkido.ecosystem

import java.awt.Color
import scala.util.Random
import com.tkido.quadtree

object Predator {
}

class Predator(var x:Double, var y:Double) extends Life{
  var radius:Double = 1.0
  var energy:Double = 1.0
  var dx:Double = 0.0
  var dy:Double = 0.0
  
  var nearX:Double = 2024.0
  var nearY:Double = 2024.0
  var seek = false

  val color:Color = new Color(255, 0, 0)
  
  def check(other:quadtree.Mover){
    if(this.equals(other))
      return
    other match{
      case grazer:Grazer =>
        if(square(x - grazer.x) + square(y - grazer.y) < square(radius + grazer.radius)){
          energy += grazer.energy / 5
          grazer.energy = 0.0
          
        }
        if(square(x - grazer.x) + square(y - grazer.y) < square(radius + 20 + grazer.radius)){
          if(square(x - grazer.x) + square(y - grazer.y) < square(x - nearX) + square(y - nearY)){
            nearX = grazer.x
            nearY = grazer.y
          }
        }
      case _ =>
    }
  }
  
  override def update{
    if(nearX == 2024.0 && nearY == 2024.0){
      if(!seek){
        seek = true
        dx = Random.nextInt(3) - 1
        dy = Random.nextInt(3) - 1
      }
    }else{
      seek = false
      dx = (nearX - x)
      dy = (nearY - y)
      val distance = math.sqrt(square(x - nearX) + square(y - nearY))
      if(distance >= 2.0){
        dx = dx / distance
        dy = dy / distance
      }
    }
    nearX = 2024.0
    nearY = 2024.0
    
    super.update
    
    if(energy >= 13.0){
      energy = 10.0
      Range(0, 3).map(_ => main.addLife(new Predator(x, y)))
    }
    
    energy *= 0.999
    energy -= 0.0005
  }
  
  override def updateCell(){
    val x1 = sanitize(x - radius - 20)
    val y1 = sanitize(y - radius - 20)
    val x2 = sanitize(x + radius + 20)
    val y2 = sanitize(y + radius + 20)
    super.updateCell(x1, y1, x2, y2)
  }
}