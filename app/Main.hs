module Main where

import Bot
import Config
import Services.Logger
import Services.Types

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
      VK -> do
        handleVK <- VK.newHandle conf
        eventSetting hLogger conf
        loopBot  (VK.initSession handleVK hLogger conf) handleVK hLogger (DataLoop (Session "" "" "0") [] M.empty "0")       
      TELEGRAM -> do
        handleTl <- TL.newHandle conf
        loopBot  (pure ()) handleTl hLogger (DataLoop (Session "" "" "0") [] M.empty "0")

