package com.tkido.ecosystem

import java.awt.Graphics2D
import scala.collection.mutable.MutableList
import scala.util.Random

import com.tkido.math.Vector
import com.tkido.collision.{Manager, Mover}
import com.tkido.tools.Logger

class Field(val length:Int) {
  val cm = new Manager()
  var count = 0
  var lives = MutableList[Life]()
  val newComers = MutableList[Life]()
  
  Range(0, 1000).map(_ => lives += new Plant(this, nextVector, Vector(0.0, 0.0)))
  Range(0, 100).map(_ => lives += new Grazer(this, nextVector))
  Range(0, 20).map(_ => lives += new Predator(this, nextVector))

  def addLife(life:Life){
    newComers += life
  }
  
  def paint(g: Graphics2D){
    for(life <- lives)
      life.paint(g)
  }
  
  def update(){
    for(life <- lives)
      life.update
    cm.check
    lives ++= newComers
    lives.filter(_.energy <= 0.0).map(cm.remove(_))
    lives = lives.filter(_.energy > 0.0)
    newComers.clear
    count += 1
    Logger.debug("Count %s :Polulation= %s".format(count, lives.size))
  }
  
  def nextVector() :Vector =
    Vector(Random.nextDouble * length, Random.nextDouble * length)
  
  def move(life:Life, x1:Double, y1:Double, x2:Double, y2:Double){
    def sanitize(x:Double) :Double = {
      x match {
        case x if (x < 0.0) => 0.0
        case x if (x >= length) => length - 0.0001
        case _ => x
      }
    }
    cm.updateCell(life,
                  sanitize(x1) / 32,
                  sanitize(y1) / 32,
                  sanitize(x2) / 32,
                  sanitize(y2) / 32)
  }
}
object Field {
  def apply(length:Int) = new Field(length)
}
