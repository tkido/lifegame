package com.tkido

import scala.collection.mutable.ArrayBuffer
import scala.math.{Pi, cos, sin}
import scala.util.Random

package object math {
  private val factMemo = ArrayBuffer[BigInt]()
  def factorial(n:Int) :BigInt = {
    if(n == 0)
      1
    else if(factMemo.isDefinedAt(n))
      factMemo(n)
    else{
      val x = factorial(n - 1) * n
      factMemo += x
      x
    }
  }
  
  def combination(m:Int, n:Int) :BigInt =
    factorial(m) / (factorial(n) * factorial(m - n))
  
  def nextVector() :Vector = {
    val rad = Random.nextDouble * 2 * Pi
    Vector(cos(rad), sin(rad))
  }
  
  def powInt(x: Int, n: Int): Int = {
    var m = n
    var a: Int = 1
    var b = x
    while (m > 0) {
      while (m % 2 == 0) {
        b *= b
        m /= 2
      }
      a *= b
      m -= 1
    }
    a
  }
  
}