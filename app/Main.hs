module Main where

-- import Bot
import Services.Config
import Services.Logger

import qualified Bot.Telegram as TL 
import qualified Bot.VK as VK
import qualified Services.Impl.Configurator as SC
import qualified Services.Impl.StdOut as SL

main :: IO ()
main = do
  hConfig <- SC.newHandle
  conf <- getConfig hConfig
  hLogger <- SL.newHandle conf
  logDebug
    hLogger
    (" -- configuration file bot.conf read:\n" ++ show conf)
  botHandle <-
    case сonfigApi conf of 
      "vk" -> VK.newHandle conf
      _ -> TL.newHandle conf
  case сonfigApi conf of
      "vk" -> VK.initSession botHandle hLogger conf
      _    -> TL.initSession botHandle hLogger
