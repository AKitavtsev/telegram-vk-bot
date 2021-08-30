{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

--
module Main where

import Bot
import Bot.Telegram
import Bot.VK
import Servises.Config
import Servises.Logger

import qualified Servises.Impl.Configurator as SC
import qualified Servises.Impl.StdOut as SL

main :: IO ()
main = do
  hConfig <- SC.newHandle
  logConf <- getLogConfig hConfig
  botConf <- getBotConfig hConfig
  hLogger <- SL.newHandle logConf
  logDebug
    hLogger
    (" -- configuration file bot.conf read:\n" ++
     show logConf ++ "\n" ++ show botConf)
  botHandle <-
    case сonfigApi botConf of
      "vk" -> Bot.VK.newHandle botConf
      _ -> Bot.Telegram.newHandle botConf
  initSession botHandle botHandle hLogger
