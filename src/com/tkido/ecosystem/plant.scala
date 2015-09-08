package com.tkido.ecosystem

import java.awt.Color
import scala.util.Random
import com.tkido.quadtree

object Plant {

}

class Plant(var x:Double, var y:Double, var dx:Double, var dy:Double) extends Life{
  var radius:Double = 1.0
  var energy:Double = 1.0
  
  var collisionCount = 0
  
  val color:Color = new Color(0, 255, 0)
  
  def check(other:quadtree.Mover){
    if(this.equals(other))
      return
    other match{
      case plant:Plant =>
        if(square(x - plant.x) + square(y - plant.y) < square(radius + plant.radius)){
          if(energy >= 10.0 && plant.energy <= 2.0)
            plant.energy = 0.0
          collisionCount += 1
        }
      case _ =>
    }
  }
  
  override def update{
    super.update
    
    dx = 0.0
    dy = 0.0
    
    energy *= (1.02 - 0.003 * collisionCount)
    collisionCount = 0
    
    if(energy >= 13.0){
      energy = 10.0
      Range(0, 3).map(_ => main.addLife(new Plant(x, y, Random.nextDouble * 40 - 20, Random.nextDouble * 40 - 20)))
    }
    energy *= 0.999
    energy -= 0.001
  }
}
