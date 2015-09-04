package com.tkido.serialize

object main extends App {
  import com.tkido.math
  import com.tkido.math.Vector
  import java.io._
  
  val v1 = Vector(1024, 99)
  val v2 = Vector(1, 10)
  println(v1)
  println(v2)
  
  val oos = new ObjectOutputStream(new FileOutputStream("data/save.dat"))
  oos.writeObject(v1)
  oos.writeObject(v2)
  oos.close
  
  val ois = new ObjectInputStream(new FileInputStream("data/save.dat"))
  val r1 = ois.readObject()
  val r2 = ois.readObject()
  println(r1)
  println(r2)
  
}