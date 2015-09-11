package com.tkido.collision

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import scala.collection.mutable.Stack

import com.tkido.tools.Logger

object Manager{
  def apply(level:Int = 5) :Manager = {
    level match{
      case 0 => new ManagerLevel0(1)
      case 1 => new ManagerLevel1(5)
      case 2 => new ManagerLevel2(21)
      case 3 => new ManagerLevel3(85)
      case 4 => new ManagerLevel4(341)
      case 5 => new ManagerLevel5(1365)
      case _ => throw new IllegalArgumentException("Split Level must be in [0..5]")
    }
  }
}

abstract class Manager(max:Int){
  private val cells = Array.fill(max)(Set[Mover]())
  private val stack = new Stack[Mover]()
  
  def remove(mover:Mover){
    cells(mover.cellNum).remove(mover)
  }
  
  def updateCell(mover:Mover, x1:Double, y1:Double, x2:Double, y2:Double){
    val upperLeft = getMortonNumber(x1, y1)
    val lowerRight = getMortonNumber(x2, y2)
    val newCellNum = getCellNumber(upperLeft, lowerRight)
    if(mover.cellNum != newCellNum){
      if(-1 != mover.cellNum)
        cells(mover.cellNum).remove(mover)
      cells(newCellNum).add(mover)
      mover.cellNum = newCellNum
    }
  }
  
  def check{
    checkCell(0)
  }
  private def checkCell(i:Int){
    val list = cells(i).toList
    for(mover <- list){
      for(other <- list)
        mover.check(other)
      for(other <- stack){
        mover.check(other)
        other.check(mover)
      }
      stack.push(mover)
    }
    val base = i * 4 + 1
    if(base < max){
      val range = Range(base, base + 4)
      for(child <- range)
        checkCell(child)
    }
    for(mover <- list){
      stack.pop
    }
  }
  
  protected def getMortonNumber(x:Int, y:Int):Int
  protected def getMortonNumber(x:Double, y:Double) :Int = {
    getMortonNumber(x.toInt, y.toInt)
  }
  protected def getCellNumber(upperLeft:Int, lowerRight:Int) :Int
   
}

class ManagerLevel0(max:Int) extends Manager(max:Int){
  protected def getMortonNumber(x:Int, y:Int):Int = 0
  protected def getCellNumber(upperLeft:Int, lowerRight:Int) :Int = 0
}

class ManagerLevel1(max:Int) extends Manager(max:Int){
  protected def getMortonNumber(x:Int, y:Int):Int = {
    x | y<<1
  }
  protected def getCellNumber(upperLeft:Int, lowerRight:Int) :Int = {
    upperLeft ^ lowerRight match{
      case n if (n & 0x3) != 0 => 0
      case _                   => lowerRight + 1
    }
  }
}

class ManagerLevel2(max:Int) extends Manager(max:Int){
  protected def getMortonNumber(x:Int, y:Int):Int = {
    def separate(arg:Int) :Int = {
      var n = arg
      n = (n|(n<<1)) & 0x55555555
      return n
    }
    return separate(x) | separate(y)<<1
  }
  protected def getCellNumber(upperLeft:Int, lowerRight:Int) :Int = {
    upperLeft ^ lowerRight match{
      case n if (n>>2 & 0x3) != 0 => 0
      case n if (n    & 0x3) != 0 => (lowerRight >> 2) + 1
      case _                      => (lowerRight >> 0) + 5
    }
  }
}

class ManagerLevel3(max:Int) extends Manager(max:Int){
  protected def getMortonNumber(x:Int, y:Int):Int = {
    def separate(arg:Int) :Int = {
      var n = arg
      n = (n|(n<<2)) & 0x33333333
      n = (n|(n<<1)) & 0x55555555
      return n
    }
    return separate(x) | separate(y)<<1
  }
  protected def getCellNumber(upperLeft:Int, lowerRight:Int) :Int = {
    upperLeft ^ lowerRight match{
      case n if (n>>4 & 0x3) != 0 => 0
      case n if (n>>2 & 0x3) != 0 => (lowerRight >> 4) + 1
      case n if (n    & 0x3) != 0 => (lowerRight >> 2) + 5
      case _                      => (lowerRight >> 0) + 21
    }
  }
}

class ManagerLevel4(max:Int) extends Manager(max:Int){
  protected def getMortonNumber(x:Int, y:Int):Int = {
    def separate(arg:Int) :Int = {
      var n = arg
      n = (n|(n<<4)) & 0x0f0f0f0f
      n = (n|(n<<2)) & 0x33333333
      n = (n|(n<<1)) & 0x55555555
      return n
    }
    return separate(x) | separate(y)<<1
  }
  protected def getCellNumber(upperLeft:Int, lowerRight:Int) :Int = {
    upperLeft ^ lowerRight match{
      case n if (n>>6 & 0x3) != 0 => 0
      case n if (n>>4 & 0x3) != 0 => (lowerRight >> 6) + 1
      case n if (n>>2 & 0x3) != 0 => (lowerRight >> 4) + 5
      case n if (n    & 0x3) != 0 => (lowerRight >> 2) + 21
      case _                      => (lowerRight >> 0) + 85
    }
  }
}

class ManagerLevel5(max:Int) extends Manager(max:Int){
  protected def getMortonNumber(x:Int, y:Int):Int = {
    def separate(arg:Int) :Int = {
      var n = arg
      n = (n|(n<<8)) & 0x00ff00ff
      n = (n|(n<<4)) & 0x0f0f0f0f
      n = (n|(n<<2)) & 0x33333333
      n = (n|(n<<1)) & 0x55555555
      return n
    }
    return separate(x) | separate(y)<<1
  }
  protected def getCellNumber(upperLeft:Int, lowerRight:Int) :Int = {
    upperLeft ^ lowerRight match{
      case n if (n>>8 & 0x3) != 0 => 0
      case n if (n>>6 & 0x3) != 0 => (lowerRight >> 8) + 1
      case n if (n>>4 & 0x3) != 0 => (lowerRight >> 6) + 5
      case n if (n>>2 & 0x3) != 0 => (lowerRight >> 4) + 21
      case n if (n    & 0x3) != 0 => (lowerRight >> 2) + 85
      case _                      => (lowerRight >> 0) + 341
    }
  }
}