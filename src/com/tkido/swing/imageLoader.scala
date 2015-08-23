package com.tkido.swing

object ImageLoader {
  import java.awt.image.BufferedImage
  import java.net.URL
  import javax.imageio.ImageIO

  def apply(path:String) :BufferedImage = {
    val imageUrl = new URL(getClass().getResource("."), "../../../../image/%s".format(path))
    return ImageIO.read(imageUrl.openConnection().getInputStream())
  }

}