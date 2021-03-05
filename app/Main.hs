{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
 
-- import Control.Monad (when)
-- import qualified Data.Map as M

import Config
import Telegram 
import VK
import Bot

import qualified Data.Configurator as C
import qualified Data.Text as T


main :: IO ()
main  = do
    conf <- getConfig
    debugM (сonfigLogg conf) "--main" 
                            (" -- configuration file bot.conf read:\n" ++ show conf)
    handle <- case сonfigApi conf of
        "vk"       -> VK.newHandle conf
        _          -> Telegram.newHandle conf
        
    sess <- (initSession handle) handle
    
    
    print sess
    
