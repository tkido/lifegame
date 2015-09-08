package com.tkido.ecosystem

import java.awt.Color
import scala.util.Random
import com.tkido.quadtree

object Grazer {

}


class Grazer(var x:Double, var y:Double) extends Life{
  var radius:Double = 1.0
  var energy:Double = 1.0
  var dx:Double = 0.0
  var dy:Double = 0.0
  
  var seek = false
  var nearX:Double = 2024.0
  var nearY:Double = 2024.0

  val color:Color = new Color(0, 0, 255)
  
  def check(other:quadtree.Mover){
    if(this.equals(other))
      return
    other match{
      case plant:Plant =>
        if(square(x - plant.x) + square(y - plant.y) < square(radius + plant.radius)){
          energy += plant.energy / 10
          plant.energy = 0.0
          
        }
        if(square(x - plant.x) + square(y - plant.y) < square(radius + 10 + plant.radius)){
          if(square(x - plant.x) + square(y - plant.y) < square(x - nearX) + square(y - nearY)){
            nearX = plant.x
            nearY = plant.y
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
      if(distance >= 1.0){
        dx = dx / distance
        dy = dy / distance
      }
    }
    nearX = 2024.0
    nearY = 2024.0
    
    super.update
    
    if(energy >= 13.0){
      energy = 10.0
      Range(0, 3).map(_ => main.addLife(new Grazer(x, y)))
    }
    
    energy *= 0.999
    energy -= 0.001
  }
  
  override def updateCell(){
    val x1 = sanitize(x - radius - 10)
    val y1 = sanitize(y - radius - 10)
    val x2 = sanitize(x + radius + 10)
    val y2 = sanitize(y + radius + 10)
    super.updateCell(x1, y1, x2, y2)
  }
}