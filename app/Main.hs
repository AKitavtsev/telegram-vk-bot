{-# LANGUAGE OverloadedStrings #-}

module Main where
 
import qualified Data.Configurator as C
import qualified Data.Text as T
import Control.Monad (when)

import Telegram 
import VK
import Config

-- import Rainbow


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
    debugM (logg conf) "--main" (" -- configuration file bot.conf read:\n" ++ show conf) 
    when (logg conf  == UNDEF) $ do
        undefM (logg conf) "" " -- logg_level By default - UNDEF"
    when (wrongToken (token conf)) $ do
        errorM (logg conf) " -- token should look like:\n " "bot1509893058:AAD3uC_cmyxDQJfBZtQgs2E4-K55xivO8Wc" 
    case api conf of
        "telegram" -> loopTelegram conf 0             
        "vk"       -> loopVk         
        _          -> warnM (logg conf) "--Unknown api." " -- By default - telegram"
    loopTelegram (conf{api = "telegram"}) 0

                 



    


