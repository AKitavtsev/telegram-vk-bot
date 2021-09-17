module Main where

import Bot
import Services.Config
import Services.Logger
import Services.Types

import qualified Bot.Telegram as TL 
import qualified Bot.VK as VK
import qualified Data.Map as M
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

  case сonfigApi conf of 
      VK -> do
        handleVK <- VK.newHandle conf
        VK.initSession handleVK hLogger conf
      TELEGRAM -> do
        handleTl <- TL.newHandle conf
        loopBot  handleTl hLogger (DataLoop (Session "" "" "0") [] M.empty "0")

