package com.tkido.lifegame

import java.awt.Cursor
import javax.imageio.ImageIO

import scala.swing._
import scala.swing.event._

object main extends SimpleSwingApplication {
  import com.tkido.lifegame.Config
  import com.tkido.tools.Logger
  
  import event.Key._
  import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle}
  import java.awt.{Color => AWTColor}
  
  Logger.level = Config.logLevel
  
  
  val fgColor = new AWTColor(48, 99, 99)
  val bgColor = new AWTColor(0, 255, 0)
  
  val ui = new AbstractUI
  
  def onKeyPress(keyCode: Value) = keyCode match {
    case Left  => ui.left()
    case Right => ui.right()
    case Up    => ui.up()
    case Down  => ui.down()
    case Space => ui.space()
    case _ =>
  }
  def onPaint(g: Graphics2D) {
    g setColor fgColor
    g drawString (ui.last, 20, 20)
  }
  
  def top = new MainFrame {
    title = "ライフゲイム"
    cursor = new Cursor(Cursor.HAND_CURSOR)
    resizable = false
    //iconImage = ImageIO.read(getClass().getResourceAsStream("/resource/favicon.bmp"))
    contents = mainPanel
  }
  def mainPanel = new Panel {
    preferredSize = new Dimension(800, 480)
    focusable = true
    listenTo(keys, this.mouse.moves)
    
    reactions += {
      case KeyPressed(_, key, _, _) =>
        onKeyPress(key)
        repaint
      case MouseEntered(source, point, modifiers) =>
        Logger.info(source, point, modifiers)
      case MouseDragged(source, point, modifiers) =>
        Logger.info(source, point, modifiers)
        
    }
    override def paint(g: Graphics2D) {
      g setColor bgColor
      g fillRect (0, 0, size.width, size.height)
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
  
  //Logger.close()
}