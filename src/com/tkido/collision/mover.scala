package com.tkido.collision

trait Mover{
  var cellNum = -1
  
  def updatePosition(x1:Float, y1:Float, x2:Float, y2:Float){
    val upperLeft = getMortonNumber(x1 / 32, y1 / 32)
    val lowerRight = getMortonNumber(x2 / 32, y2 / 32)
    val newCellNum = getCellNumber(upperLeft, lowerRight)
    if(cellNum != newCellNum){
      if(-1 != cellNum)
        cells(cellNum).remove(this)
      cells(newCellNum).add(this)
      cellNum = newCellNum
    }
  }
  
  def check(other:Mover)
}