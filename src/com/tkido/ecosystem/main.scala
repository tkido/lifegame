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