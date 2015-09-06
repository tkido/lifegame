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
    }else if(x + radius >= fieldWidth){
      x = 2 * fieldWidth -2 * radius - x
      dx *= -1
    }
    
    y += dy
    if(y - radius < 0.0){
      y = 2 * radius - y
      dy *= -1
    }else if(y + radius >= fieldHeight){
      y = 2 * fieldHeight -2 * radius - y
      dy *= -1
    }
    
    val x1 = x match {
      case x if x - radius < 0.0 => 0.0
      case x => x - radius
    }
    val y1 = y match {
      case y if y - radius < 0.0 => 0.0
      case y => y - radius
    }
    val x2 = x match {
      case x if x + radius >= fieldWidth => fieldWidth - 0.0001
      case x => x + radius
    }
    val y2 = y match {
      case y if y + radius >= fieldHeight => fieldHeight - 0.0001
      case y => y + radius
    }
    updatePosition(x1, y1, x2, y2)
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
      case other:Plant =>
        if(square(x - other.x) + square(y - other.y) < square(radius + other.radius))
          collisionCount += 1
    }
  }
  
  override def update{
    super.update
    
    dx = 0.0
    dy = 0.0
    energy *= 1.01 - 0.0017 * collisionCount
    collisionCount = 0
    
    if(energy >= 15){
      energy = 10
      Range(0, 5).map(_ => main.addLife(new Plant(x, y, Random.nextDouble * 20 - 10, Random.nextDouble * 20 - 10)))
    }
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

  val bgColor = new Color(255, 255, 255)
  
  val ui = new AbstractUI
  
  val icon = ImageLoader("favicon.bmp")
  
  var lives = MutableList[Life]()
  Range(0, 100).map(_ => lives += new Plant(Random.nextDouble * fieldWidth, Random.nextDouble * fieldHeight, 0.0, 0.0))
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
    preferredSize = new Dimension(fieldWidth, fieldHeight)
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
      Logger.debug("Polulation before = %s" format lives.size)
      lives = lives.filter(_.energy > 0.0)
      Logger.debug("Polulation after = %s" format lives.size)
      newComers.clear
    }
  }
    
}