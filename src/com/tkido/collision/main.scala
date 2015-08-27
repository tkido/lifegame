package com.tkido.collision

import java.awt.Cursor

import scala.swing._
import scala.swing.event._
import scala.swing.event.Key._
import scala.util.Random
import java.awt.geom.Ellipse2D



object Bullet{
  val normalColor = new Color(0, 0, 255)
  val collisionedColor = new Color(255, 0, 0)
}
class Bullet(val radius:Double, var x:Double, var y:Double, var dx:Double, var dy:Double, var collisioned:Boolean){
  def square(x:Double) :Double = x * x
  
  def paint(g: Graphics2D) {
    g.setColor(if(collisioned) Bullet.collisionedColor else Bullet.normalColor)
    g.fill(new Ellipse2D.Double(x - radius, y - radius, radius * 2 , radius * 2))
  }
  
  def update{
    x += dx
    if(x < 0.0){
      x *= -1
      dx *= -1
    }else if(x >= fieldWidth){
      x = 2 * fieldWidth - x
      dx *= -1
    }
    
    y += dy
    if(y < 0.0){
      y *= -1
      dy *= -1
    }else if(y >= fieldHeight){
      y = 2 * fieldHeight - y
      dy *= -1
    }
    
    collisioned = false
  }
  
  def check(other:Bullet){
    if(!this.equals(other))
      if(square(x - other.x) + square(y - other.y) < square(2 * radius))
        collisioned = true
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
  
  val bullets = Range(0, 1000).map(_ => new Bullet(5, Random.nextDouble * fieldWidth, Random.nextDouble * fieldHeight, Random.nextDouble * 4 - 2, Random.nextDouble * 4 - 2, false))

  
  def onKeyPress(keyCode: Value) = keyCode match {
    case Key.Space => ui.space()
    case _ =>
  }
  
  def onPaint(g: Graphics2D) {
    g setColor fgColor
    
    for(bullet <- bullets)
      bullet.paint(g)
    
  }
  
  def top = new MainFrame { frame =>
    title = "衝突判定"
    cursor = new Cursor(Cursor.HAND_CURSOR)
    resizable = false
    iconImage = ImageLoader("favicon.bmp")
    contents = mainPanel
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
      for(bullet <- bullets)
        for(other <- bullets)
          bullet.check(other)
    }
  }
    
}