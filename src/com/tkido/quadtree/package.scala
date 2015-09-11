package com.tkido

package object quadtree {
  import scala.collection.mutable.Set
  import scala.collection.mutable.Stack
  
  import com.tkido.tools.Logger
  
  private val cells = Array.fill(1365)(Set[Mover]())
  private val stack = new Stack[Mover]()
  
  trait Mover{
    var cellNum = -1
    def check(other:Mover)
  }
  
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
    if(base < 1365){
      val range = Range(base, base + 4)
      for(child <- range)
        checkCell(child)
    }
    for(mover <- list){
      stack.pop
    }
  }
  
  private def getMortonNumber(x:Int, y:Int):Int = {
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
  private def getMortonNumber(x:Double, y:Double) :Int = {
    getMortonNumber(x.toInt, y.toInt)
  }
  
  private def getCellNumber(upperLeft:Int, lowerRight:Int) :Int = {
    upperLeft ^ lowerRight match{
      case n if (n>>8 & 0x3) != 0 => 0                        //L0
      case n if (n>>6 & 0x3) != 0 => (lowerRight >>  8) + 1   //L1
      case n if (n>>4 & 0x3) != 0 => (lowerRight >>  6) + 5   //L2
      case n if (n>>2 & 0x3) != 0 => (lowerRight >>  4) + 21  //L3
      case n if (n    & 0x3) != 0 => (lowerRight >>  2) + 85  //L4
      case _                      => (lowerRight >>  0) + 341 //L5
    }
  }
  
  
}