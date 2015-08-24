package com.tkido.lifegame

import java.awt.Cursor

import scala.swing._
import scala.swing.event._
import scala.swing.event.Key._

object main extends SimpleSwingApplication {
  import com.tkido.lifegame.Config
  import com.tkido.swing.ImageLoader
  import com.tkido.tools.Logger
  
  import java.awt.{Dimension, Graphics2D, Graphics, Image, Point, Rectangle}
  import java.awt.{Color => AWTColor}
  
  Logger.level = Config.logLevel
  
  val fgColor = new AWTColor(0, 0, 0)
  val bgColor = new AWTColor(255, 255, 255)
  
  val ui = new AbstractUI
  val grid = Grid(colNum, rowNum)
  val icon = ImageLoader("favicon.bmp")
  
  def onKeyPress(keyCode: Value) = keyCode match {
    case Key.Left  => ui.left()
    case Key.Right => ui.right()
    case Key.Up    => ui.up()
    case Key.Down  => ui.down()
    case Key.Space => grid.update //ui.space()
    case Key.R => grid.reset
    case Key.S => grid.shuffle
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
  
  def top = new MainFrame { frame =>
    title = "ライフゲイム"
    cursor = new Cursor(Cursor.HAND_CURSOR)
    resizable = false
    iconImage = ImageLoader("favicon.bmp")
    val mp = mainPanel
    contents = mp
    menuBar = new MenuBar{
      contents += new Menu("ファイル(F)") {
        mnemonic = Key.F

        contents += new MenuItem(Action("リセット(R)") {
          grid.reset
          mp.repaint
        }) {
          mnemonic = Key.N
        }
        contents += new MenuItem(Action("終了(X)") {
          frame.dispose()
        }) {
          mnemonic = Key.X
        }
      }
      contents += new Menu("編集(E)") {
        mnemonic = Key.E

        contents += new MenuItem(Action("シャッフル(S)") {
          grid.shuffle
          mp.repaint
        }) {
          mnemonic = Key.S
        }
      }
    }
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
          g drawImage (icon, null, v.x * cellWidth, v.y * cellHeight)
          //g.fillRect(v.x * cellWidth, v.y * cellHeight, cellWidth, cellHeight)
      
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