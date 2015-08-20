package com.tkido.lifegame

object main extends App {
  import com.tkido.lifegame.Config
  import com.tkido.tools.Logger
  
  Logger.level = Config.logLevel
  
  

  Logger.close()
}
