package com.tkido

package object math {
  private val factMemo:Array[BigInt] = Array()
  def factorial(n:Int) :BigInt = {
    if(n == 0)
      1
    else if(factMemo.isDefinedAt(n))
      factMemo(n)
    else{
      val x = factorial(n - 1) * n
      factMemo :+ x
      x
    }
  }
  
  def combination(m:Int, n:Int) :BigInt = {
    factorial(m) / (factorial(n) * factorial(m - n))
  }
}