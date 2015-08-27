package com.tkido

package object quadtree {
  import scala.collection.mutable.Set
  
  trait Quadable{
    var currentCellNum = 0
    def updatePosition(x1:Double, y1:Double, x2:Double, y2:Double){
      val upperLeft = getMortonNumber(x1 / 32, y1 / 32)
      val lowerRight = getMortonNumber(x2 / 32, y2 / 32)
      val newCellNum = getCellNumber(upperLeft, lowerRight)
      if(currentCellNum != newCellNum){
        cells(currentCellNum).remove(this)
        cells(newCellNum).add(this)
        currentCellNum = newCellNum
      }
    }
  }
  
  class Cell(){
    private val set = Set[Quadable]()
    def add(elem:Quadable){
      set.add(elem)
    }
    def remove(elem:Quadable){
      set.remove(elem)
    }
  }
  
  def cells = Array.fill(1365)(new Cell)
  
  def getMortonNumber(x:Int, y:Int):Int = {
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
  def getMortonNumber(x:Double, y:Double) :Int = {
    getMortonNumber(x.toInt, y.toInt)
  }
  
  def getCellNumber(upperLeft:Int, lowerRight:Int) :Int = {
    val n = upperLeft ^ lowerRight
    if((n>>8 & 0x000003) != 0) return (lowerRight >> 10)       //L0
    if((n>>6 & 0x000003) != 0) return (lowerRight >>  8) + 1   //L1
    if((n>>4 & 0x000003) != 0) return (lowerRight >>  6) + 5   //L2
    if((n>>2 & 0x000003) != 0) return (lowerRight >>  4) + 21  //L3
    if((n    & 0x000003) != 0) return (lowerRight >>  2) + 85  //L4
    if(n == 0)                 return (lowerRight >>  0) + 341 //L5
    return 0
  }
  
  
}