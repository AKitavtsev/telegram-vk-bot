module Main where

import Bot
import Bot.Telegram
import Bot.VK
import Services.Config
import Services.Logger

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
      "vk" -> Bot.VK.newHandle conf
      _ -> Bot.Telegram.newHandle conf
  initSession botHandle botHandle hLogger
