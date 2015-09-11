package com.tkido.ecosystem

import scala.swing._
import scala.swing.event._
import scala.swing.event.Key._

import java.awt.Cursor
import java.awt.Color
import java.awt.{Dimension, Graphics2D, Image}

import com.tkido.collision.Config
import com.tkido.swing.ImageLoader
import com.tkido.tools.Logger


object main extends SimpleSwingApplication {
  Logger.level = Config.logLevel
  val fieldLength = 1024
  val field = Field(fieldLength)
  val bgColor = new Color(255, 255, 255)
  val ui = new AbstractUI
  val icon = ImageLoader("favicon.bmp")
  
  
  def onKeyPress(keyCode: Value) = keyCode match {
    case Key.Space => ui.space()
    case Key.Enter =>
    case _ =>
  }
  
  def onPaint(g: Graphics2D){
    field.paint(g)
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
      field.update
    }
  }
    
}