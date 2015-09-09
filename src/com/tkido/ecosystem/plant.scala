package com.tkido.ecosystem

import java.awt.Color
import scala.util.Random
import com.tkido.math.Vector
import com.tkido.quadtree.Mover

object Plant {
  val color:Color = new Color(0, 255, 0)
}

class Plant(var v:Vector, var dv:Vector) extends Life{
  var radius:Double = 1.0
  var energy:Double = 1.0
  
  var collisionCount = 0
  
  val color:Color = Plant.color
    
  def check(other:Mover){
    if(this.equals(other))
      return
    other match{
      case p:Plant =>
        if((v - p.v).size < radius + p.radius){
          if(energy >= 10.0 && p.energy < 2.0)
            p.energy = 0.0
          collisionCount += 1
        }
      case _ =>
    }
  }
  
  override def update{
    super.update
    
    dv = Vector(0.0, 0.0)
    
    energy *= (1.02 - 0.003 * collisionCount)
    collisionCount = 0
    
    if(energy >= 13.0){
      energy = 10.0
      Range(0, 3).map(_ => main.addLife(new Plant(v, Vector(Random.nextDouble * 100 - 50, Random.nextDouble * 100 - 50))))
    }
    energy *= 0.995
    energy -= 0.01
  }
}
