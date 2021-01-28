{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
 
import Control.Monad (when)

import Config
import Telegram 
import VK

import qualified Data.Configurator as C
import qualified Data.Text as T


-- Just (Response {result = [Update {update_id = 146878273, 
                                  -- message = Just (Message {message_id = 25, 
                                                           -- from = Just (User {user_id = 484189456,
                                                                              -- user_is_bot = False,
                                                                              -- user_first_name = "Андрей"}),
                                                                  -- date = 1610909106,
                                                                  -- chat = Chat {chat_id = 484189456,
                                                                               -- chat_type = Private}
                                                           -- }
                                                  -- )
                                  -- }
                         -- ]
                 -- }
       -- )                
 
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
        "telegram" -> loopTelegram conf 0
        "vk"       -> loopVk
        _          -> warnM (сonfigLogg conf) "--Unknown api." " -- By default - telegram"
    loopTelegram (conf{сonfigApi = "telegram"}) 0

                 



    


