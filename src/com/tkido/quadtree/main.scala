package com.tkido.quadtree


object main extends App {
  import com.tkido.tools.Logger
  Logger.level = 0
  
  //Logger.debug(getMortonNumber(3,6))
  
  //Logger.debug(getCellNumber(19,31))
  //Logger.debug(getMortonNumber(0, 0))
  //Logger.debug(getMortonNumber(31.5, 31.5))
  //Logger.debug(getCellNumber(0, 1023))

  //Logger.debug(getMortonNumber(16.5, 0.5))
  //Logger.debug(getMortonNumber(31.5, 15.5))
  Logger.debug(getCellNumber(256, 511))
  
  //Logger.debug(getCellNumber(15, 60))
  //Logger.debug(getCellNumber(44, 47))
  Logger.debug(getCellNumber(0, 1023))
  
  Logger.debug(getCellNumber(1023, 1023))
  
  Logger.close()
}