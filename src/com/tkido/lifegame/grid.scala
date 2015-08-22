package com.tkido.lifegame

class Grid(val width:Int, val height:Int, d:Int) {
  import scala.collection.immutable.SortedSet  
  
  private val arrays = Array.fill[Int](height, width)(d)
  
  def apply(i:Int, j:Int) =
    arrays(j)(i)
  def update(i:Int, j:Int, value:Int) =
    arrays(j)(i) = value
  
  def apply(v:Vector) =
    arrays(v.y)(v.x)
  def update(v:Vector, value:Int) =
    arrays(v.y)(v.x) = value

  def +(that:Grid) :Grid = {
    for (j <- Range(0, height))
      for (i <- Range(0, width))
        arrays(j)(i) += that(i, j)
    this
  }
  def -(that:Grid) :Grid = {
    for (j <- Range(0, height))
      for (i <- Range(0, width))
      arrays(j)(i) -= that(i, j)
    this
  }
  
  def foreach(f: Int => Unit): Unit =
    for(row <- arrays; cell <- row) f(cell)

  def map(f: Int => Int): Grid = {
    val grid = Grid(width, height)
    for (j <- Range(0, height))
      for (i <- Range(0, width))
        grid(i, j) = f(arrays(j)(i))
    grid
  }
  
  def findPointOf(f: Int => Boolean): Option[Vector] = {
    for (j <- Range(0, height))
      for (i <- Range(0, width))
        if (f(arrays(j)(i))) return Some(Vector(i, j))
    None
  }
  def findPointsOf(f: Int => Boolean): Set[Vector] = {
    var result = SortedSet[Vector]()
    for (j <- Range(0, height))
      for (i <- Range(0, width))
        if (f(arrays(j)(i))) result += Vector(i, j)
    result
  }

  def vectors = new Iterator[Vector] {
    var x = 0; var y = 0
    def hasNext = (y != height)
    def next = {
      val ret = Vector(x, y)
      x += 1
      if (x == width){x = 0; y += 1}
      ret
    }
  }
  def zipWithVector = new Iterator[(Int, Vector)] {
    var x = 0; var y = 0
    def hasNext = (y != height)
    def next = {
      val ret = (arrays(y)(x), Vector(x, y))
      x += 1
      if (x == width){x = 0; y += 1}
      ret
    }
  }
  
  override def toString = toString(2)
  def toString(width:Int) = {
    val templete = "%%%dd".format(width)
    (for (row <- arrays) yield {
      (for(cell <- row) yield {
        templete.format(cell)
      }).mkString("")
    }).mkString("\n") + "\n"
  }
  
  def update{
    for (j <- Range(0, height))
      for (i <- Range(0, width))
        if(arrays(j)(i) >= 10){
          for(dy <- List(-1, 0, 1))
            for(dx <- List(-1, 0, 1)){
              val ay = j + dy match{
                case -1 => height - 1
                case `height` => 0
                case _ => j + dy
              }
              val ax = i + dx match{
                case -1 => width - 1
                case `width` => 0
                case _ => i + dx
              }
              arrays(ay)(ax) += 1
            }
        }
    for (j <- Range(0, height))
      for (i <- Range(0, width)){
        arrays(j)(i) = arrays(j)(i) match {
          case 3 | 13 | 14 => 10
          case _ => 0
        }
      }
  }
}

object Grid{  
  def apply(w:Int, h:Int) = new Grid(w, h, 0)
  def apply(w:Int, h:Int, default:Int) = new Grid(w, h, default)
  
}