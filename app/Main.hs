module Main where

import Bot
import Config
import Services.Logger

import qualified Bot.Telegram as TL 
import qualified Bot.VK as VK
import qualified Data.Map as M
import qualified Services.Impl.StdOut as SL

main :: IO ()
main = do
  conf <- getConfig
  hLogger <- SL.newHandle conf
  logDebug
    hLogger
    (" -- configuration file bot.conf read:\n" ++ show conf)

  case сonfigApi conf of 
      "vk" -> do
        handleVK <- VK.newHandle conf
        VK.eventSetting hLogger conf
        VK.initSession handleVK hLogger conf
      _ -> do
        handleTl <- TL.newHandle conf
        loopBot  handleTl hLogger (DataLoop (Session "" "" "0") [] M.empty "0")

