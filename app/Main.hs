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


-- [Update {update_id = 146878330,
         -- message = Just (Message {message_id = 261,
                                  -- from = Just (User {user_id = 484189456, user_is_bot = False, user_first_name = "\1040\1085\1076\1088\1077\1081"}), text = Just "222"})}]

main :: IO ()
main  = do
    conf <- getConfig
    debugM (сonfigLogg conf) "--main" (" -- configuration file bot.conf read:\n" ++ show conf) 
    when (сonfigLogg conf  == UNDEF) $ do
        undefM (сonfigLogg conf) "" " -- logg_level By default - UNDEF"
    when (сonfigToken conf == "") $ do
        errorM (сonfigLogg conf)  "The token for teltgram is not defined in the configuration file.\n "
                            "Default not posible"
    when (wrongToken (сonfigToken conf)) $ do
        errorM (сonfigLogg conf) " -- token should look like:\n " "bot1509893058:AAD3uC_cmyxDQJfBZtQgs2E4-K55xivO8Wc" 
    case сonfigApi conf of
        "telegram" -> loopTelegram conf M.empty 0
        "vk"       -> loopVk
        _          -> warnM (сonfigLogg conf) "--Unknown api." " -- By default - telegram"
    loopTelegram conf{сonfigApi = "telegram"} M.empty 0

                 



    


