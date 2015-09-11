package com.tkido.ecosystem

import java.awt.Graphics2D
import scala.collection.mutable.MutableList
import scala.util.Random

import com.tkido.math.Vector
import com.tkido.quadtree
import com.tkido.tools.Logger

class Field(val length:Int) {
  var count = 0
  var lives = MutableList[Life]()
  Range(0, 1000).map(_ => lives += new Plant(this, nextVector, Vector(0.0, 0.0)))
  Range(0, 100).map(_ => lives += new Grazer(this, nextVector))
  Range(0, 20).map(_ => lives += new Predator(this, nextVector))
  val newComers = MutableList[Life]()

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
    quadtree.checkCell(0)
    lives ++= newComers
    lives.filter(_.energy <= 0.0).map(_.remove)
    lives = lives.filter(_.energy > 0.0)
    newComers.clear
    count += 1
    Logger.debug("Count %s :Polulation= %s".format(count, lives.size))
  }
  
  def sanitize(x:Double) :Double = {
    x match {
      case x if (x < 0.0) => 0.0
      case x if (x >= length) => - 0.0001
      case _ => x
    }
  }
  
  def nextVector() :Vector =
    Vector(Random.nextDouble * length, Random.nextDouble * length)
  
  
}
object Field {
  def apply(length:Int) = new Field(length)
}
