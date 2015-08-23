package com.tkido.lifegame

import java.awt.Cursor

import scala.swing._
import scala.swing.event._

object main extends SimpleSwingApplication {
  import com.tkido.lifegame.Config
  import com.tkido.swing.ImageLoader
  import com.tkido.tools.Logger
  
  import event.Key._
  import java.awt.{Dimension, Graphics2D, Graphics, Image, Point, Rectangle}
  import java.awt.{Color => AWTColor}
  
  Logger.level = Config.logLevel
  
  val fgColor = new AWTColor(0, 0, 0)
  val bgColor = new AWTColor(255, 255, 255)
  
  val ui = new AbstractUI
  val grid = Grid(colNum, rowNum)
  
  def onKeyPress(keyCode: Value) = keyCode match {
    case Left  => ui.left()
    case Right => ui.right()
    case Up    => ui.up()
    case Down  => ui.down()
    case Space => grid.update //ui.space()
    case R => grid.reset
    case S => grid.shuffle
    case _ =>
  }

  def onPaint(g: Graphics2D) {
    g setColor fgColor
    g drawString (ui.last, 20, 20)
  }
  
  def onDrag(point:Point, modifiers:Key.Modifiers) :Boolean = {
    val to = modifiers match {
          case 1024 => 10
          case 4096 => 0
    }
    val x = point.x / cellWidth
    val y = point.y / cellWidth
    
    if(grid(x, y) == to){
      return false
    }else{
      grid(x, y) = to
      return true
    }
  }
  
  def top = new MainFrame {
    title = "ライフゲイム"
    cursor = new Cursor(Cursor.HAND_CURSOR)
    resizable = false
    iconImage = ImageLoader("favicon.bmp")
    contents = mainPanel
  }
  def mainPanel = new Panel {
    preferredSize = new Dimension(cellWidth * colNum, cellHeight * rowNum)
    focusable = true
    listenTo(keys, this.mouse.moves)
    
    reactions += {
      case KeyPressed(_, key, _, _) =>
        onKeyPress(key)
        repaint
      case MouseDragged(source, point, modifiers) =>
        //Logger.info(source, point, modifiers)
        modifiers match {
          case 1024 | 4096 =>
            if(onDrag(point, modifiers))
              repaint
        }
    }
    override def paint(g: Graphics2D) {
      g setColor bgColor
      g fillRect (0, 0, size.width, size.height)
      g setColor fgColor
      
      for((cell, v) <- grid.zipWithVector)
        if (cell >= 10)
          g.fillRect(v.x * cellWidth, v.y * cellHeight, cellWidth, cellHeight)
      
      onPaint(g)
    }
  }
  
  class AbstractUI {
    private[this] var lastKey: String = ""
  
    def left() {
      lastKey = "left"
    }
    def right() {
      lastKey = "right"
    }
    def up() {
      lastKey = "up"
    }
    def down() {
      lastKey = "down"
    }
    def space() {
      lastKey = "space"
    }
    def last: String = lastKey
  }
  
}