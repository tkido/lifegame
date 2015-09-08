package com.tkido.ecosystem

import java.awt.Cursor
import scala.math

import scala.swing._
import scala.swing.event._
import scala.swing.event.Key._
import scala.util.Random
import java.awt.geom.Ellipse2D

import com.tkido.quadtree
import com.tkido.tools.Logger


abstract class Life() extends quadtree.Mover{
  var x:Double
  var y:Double
  var dx:Double
  var dy:Double
  var radius:Double
  var energy:Double
  
  val color:Color
  
  protected def square(x:Double) :Double = x * x
  protected def sanitize(x:Double) :Double = {
    x match {
      case x if x < 0.0 => 0.0
      case x if x >= fieldLength => - 0.0001
      case _ => x
    }
  }
  
  def paint(g: Graphics2D) {
    g.setColor(color)
    g.fill(new Ellipse2D.Double(x - radius, y - radius, radius * 2 , radius * 2))
  }
  
  def update{
    radius = math.sqrt(energy)
    
    x += dx
    if(x - radius < 0.0){
      x = 2 * radius - x
      dx *= -1
    }else if(x + radius >= fieldLength){
      x = 2 * fieldLength -2 * radius - x
      dx *= -1
    }
    
    y += dy
    if(y - radius < 0.0){
      y = 2 * radius - y
      dy *= -1
    }else if(y + radius >= fieldLength){
      y = 2 * fieldLength -2 * radius - y
      dy *= -1
    }
    
    updateCell()
  }
  
  def updateCell(){
    val x1 = sanitize(x - radius)
    val y1 = sanitize(y - radius)
    val x2 = sanitize(x + radius)
    val y2 = sanitize(y + radius)
    super.updateCell(x1, y1, x2, y2)
  }
  
  def check(other:quadtree.Mover)
  
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



object main extends SimpleSwingApplication {
  import scala.collection.mutable.MutableList
  
  import com.tkido.collision.Config
  import com.tkido.swing.ImageLoader
  import com.tkido.tools.Logger
  
  import java.awt.{Dimension, Graphics2D, Graphics, Image, Point, Rectangle}
  import java.awt.Color
  
  Logger.level = Config.logLevel
  var count = 0
  
  val bgColor = new Color(255, 255, 255)
  
  val ui = new AbstractUI
  
  val icon = ImageLoader("favicon.bmp")
  
  var lives = MutableList[Life]()
  Range(0, 2000).map(_ => lives += new Plant(Random.nextDouble * fieldLength, Random.nextDouble * fieldLength, 0.0, 0.0))
  Range(0, 100).map(_ => lives += new Grazer(Random.nextDouble * fieldLength, Random.nextDouble * fieldLength))
  Range(0, 50).map(_ => lives += new Predator(Random.nextDouble * fieldLength, Random.nextDouble * fieldLength))
  val newComers = MutableList[Life]()

  def addLife(life:Life){
    newComers += life
  }
  
  def onKeyPress(keyCode: Value) = keyCode match {
    case Key.Space => ui.space()
    case Key.Enter =>
    case _ =>
  }
  
  def onPaint(g: Graphics2D) {
    for(life <- lives)
      life.paint(g)
    
  }
  
  def top = new MainFrame { frame =>
    title = "生態系"
    cursor = new Cursor(Cursor.HAND_CURSOR)
    resizable = false
    iconImage = ImageLoader("favicon.bmp")
    contents = mainPanel
    val timer = new javax.swing.Timer(5, Swing.ActionListener(e =>
    {
      ui.space
      frame.repaint
    }))
    timer.start
  }
  def mainPanel = new Panel {
    preferredSize = new Dimension(fieldLength, fieldLength)
    focusable = true
    listenTo(keys, this.mouse.moves)
    
    reactions += {
      case KeyPressed(_, key, _, _) =>
        onKeyPress(key)
        repaint
    }
    override def paint(g: Graphics2D) {
      g setColor bgColor
      g fillRect (0, 0, size.width, size.height)
      
      onPaint(g)
    }
  }
  
  class AbstractUI {
    def space() {
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
  }
    
}