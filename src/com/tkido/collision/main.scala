package com.tkido.collision

import java.awt.Cursor

import scala.swing._
import scala.swing.event._
import scala.swing.event.Key._
import scala.util.Random
import java.awt.geom.Ellipse2D

import com.tkido.quadtree
import com.tkido.tools.Logger

object Bullet{
  val normalColor = new Color(0, 0, 255)
  val collisionedColor = new Color(255, 0, 0)
}
class Bullet(val radius:Float, var x:Float, var y:Float, var dx:Float, var dy:Float, var collisioned:Boolean) extends quadtree.Mover{
  override def toString :String = {
    "(%f,%f)".format(x, y)
  }
  def square(x:Float) :Float = x * x
  
  def paint(g: Graphics2D) {
    g.setColor(if(collisioned) Bullet.collisionedColor else Bullet.normalColor)
    g.fill(new Ellipse2D.Float(x - radius, y - radius, radius * 2 , radius * 2))
  }
  
  def update{
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
    
    collisioned = false
    
    val x1 = x match {
      case x if x - radius < 0.0f => 0.0f
      case x => x - radius
    }
    val y1 = y match {
      case y if y - radius < 0.0f => 0.0f
      case y => y - radius
    }
    val x2 = x match {
      case x if x + radius >= fieldWidth => fieldWidth - 0.0001f
      case x => x + radius
    }
    val y2 = y match {
      case y if y + radius >= fieldHeight => fieldHeight - 0.0001f
      case y => y + radius
    }
    //updateCell(x1, y1, x2, y2)
    //Logger.debug(cellNum)
  }
  
  def check(other:quadtree.Mover){
    if(this.equals(other))
      return
    other match{
      case other:Bullet =>
        if(square(x - other.x) + square(y - other.y) < square(radius + other.radius))
          collisioned = true
    }
  }
}


object main extends SimpleSwingApplication {
  import com.tkido.collision.Config
  import com.tkido.swing.ImageLoader
  import com.tkido.tools.Logger
  
  import java.awt.{Dimension, Graphics2D, Graphics, Image, Point, Rectangle}
  import java.awt.Color
  
  Logger.level = Config.logLevel
  
  val fgColor = new Color(0, 0, 0)
  val bgColor = new Color(255, 255, 255)
  
  val ui = new AbstractUI
  
  val icon = ImageLoader("favicon.bmp")
  
  val bullets = Range(0, 1000).map(_ => new Bullet(Random.nextInt(15)+5, Random.nextFloat * fieldWidth, Random.nextFloat * fieldHeight, Random.nextFloat * 4 - 2, Random.nextFloat * 4 - 2, false))

  
  def onKeyPress(keyCode: Value) = keyCode match {
    case Key.Space => ui.space()
    case Key.Enter =>
    case _ =>
  }
  
  def onPaint(g: Graphics2D) {
    for(bullet <- bullets)
      bullet.paint(g)
    
  }
  
  def top = new MainFrame { frame =>
    title = "衝突判定"
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
      for(bullet <- bullets)
        bullet.update
      quadtree.checkCell(0)
    }
  }
    
}