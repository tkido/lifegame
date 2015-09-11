package com.tkido.collision

trait Mover{
  var cellNum = -1
  def check(other:Mover)
}