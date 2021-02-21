{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
 
import Control.Monad (when)
import qualified Data.Map as M

import Config
import Telegram 
import VK

import qualified Data.Configurator as C
import qualified Data.Text as T


main :: IO ()
main  = do
    conf <- getConfig
    debugM (сonfigLogg conf) "--main" 
                            (" -- configuration file bot.conf read:\n" ++ show conf) 
    case сonfigApi conf of
        "telegram" -> loopTelegram conf M.empty 0
        "vk"       -> initSession conf
        _          -> warnM (сonfigLogg conf) "-- Main   " "-- Unknown api. By default - telegram"
    loopTelegram conf{сonfigApi = "telegram"} M.empty 0
