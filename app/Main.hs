{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Telegram 
import VK
import Bot
import Log
import Logger

import qualified Data.Map as M
import qualified Data.Configurator as C
import qualified Data.Text as T


main :: IO ()
main  = do
    handleLog <- Logger.newHandle
    -- let debugM = Log.debugM handleL
    conf <- getConfig handleLog
    (Log.debugM handleLog) (сonfigLogg conf) "--main" 
                            (" -- configuration file bot.conf read:\n" ++ show conf)
    handle <- case сonfigApi conf of
        "vk"       -> VK.newHandle conf handleLog
        _          -> Telegram.newHandle conf handleLog
        
    sess <- (initSession handle) handle    
    loopBot handle sess M.empty
    

    
