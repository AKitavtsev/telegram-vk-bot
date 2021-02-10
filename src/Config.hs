{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
    where

import Control.Monad (when)
import Data.List (all)
import Data.Char
 
import qualified Data.Configurator as C
import qualified Data.Text as T


data LoggLevel = DEBUG | INFO | WARN | ERROR |UNDEF deriving (Show, Eq, Ord)

debugM :: LoggLevel -> String -> String -> IO ()
debugM logg pref str = do
    when (logg <= DEBUG) $ do
        putStrLn ("DEBUG  " ++ pref ++ "   " ++ str)
        return ()

infoM :: LoggLevel -> String -> String -> IO ()
infoM logg pref str = do
    when (logg <= INFO) $ do
        putStrLn ("INFO   " ++ pref ++ "   " ++ str)
        return ()

warnM :: LoggLevel -> String -> String -> IO ()
warnM logg pref str = do
    when (logg <= WARN) $ do
        putStrLn ("WARN   " ++ pref ++ "   " ++ str)
        return ()

errorM :: LoggLevel -> String -> String -> IO ()
errorM logg pref str = do
    when (logg  <= ERROR) $ do
        putStrLn ("ERROR  " ++ pref ++ "   " ++ str)
        return ()

undefM :: LoggLevel -> String -> String -> IO ()
undefM logg pref str = do
        putStrLn ("UNDEF  " ++ pref ++ str)
        return ()         
        
data Config = Config {сonfigApi          :: !String 
                     ,сonfigToken        :: !String
                     ,сonfigLogg         :: !LoggLevel
                     ,сonfigNumberRepeat :: !Int
                     ,messageForRepeat   :: !String
                     ,messageForHelp     :: !String
                     ,myTimeout          :: !Int
                     } deriving (Show)
    
getConfig :: IO Config
getConfig = do
    conf  <- C.load [C.Optional "bot.conf", C.Optional "local_bot.conf"]
    api   <- C.lookupDefault "telegram" conf (T.pack "configBot.api") :: IO String    
   
 -- token <- C.lookupDefault "" conf (T.pack "configBot.token") :: IO String

    loggS <- C.lookupDefault "INFO" conf (T.pack "configBot.logg_level") :: IO String 
    let loggLevel = case loggS of
                        "DEBUG" -> DEBUG
                        "WARN"  -> WARN
                        "INFO"  -> INFO
                        "ERROR" -> ERROR
                        _       -> UNDEF
    when (loggLevel  == UNDEF) $ do
        undefM loggLevel "-- config" " -- logg_level By default - UNDEF"
    token <- getToken conf api loggLevel
    numberRepeat <- C.lookupDefault 1 conf (T.pack "configBot.repeat") :: IO Int
    messageForRepeat <- C.lookupDefault "" 
                        conf (T.pack "configBot.messageForRepeat") :: IO String
    messageForHelp <- C.lookupDefault "" 
                        conf (T.pack "configBot.messageForHelp") :: IO String
    myTimeout <- C.lookupDefault 5 conf (T.pack "configBot.timeout") :: IO Int
    
    
    return (Config api token loggLevel numberRepeat
                   messageForRepeat messageForHelp myTimeout)
        where
          getToken conf "telegram" loggLevel = do
            token <- C.lookupDefault "" conf (T.pack "configBot.token") :: IO String
            when (wrongToken token) $ do
              errorM loggLevel " -- token for telegram should look like:\n "
                        "bot1509893058:AAD3uC_cmyxDQJfBZtQgs2E4-K55xivO8Wc"
            return token
          getToken conf "vk" loggLevel = do
            token <- C.lookupDefault "" conf (T.pack "configBot.vkToken") :: IO String
            when (vkWrongToken token) $ do
              errorM loggLevel " -- token for vk should look like:\n "                "f471666483f81526e052c193223df886e08x1de38a7b823d4614ec44f3z680ffce51f29f177d3s6be664y"
            return token
          getToken _ _ loggLevel = do
            errorM loggLevel "-- config" " -- api should be telegran or vk"
            return ""
            
wrongToken :: String -> Bool
wrongToken ('b':'o':'t':xs) = not (length xs == 46)
wrongToken _ = True

vkWrongToken :: String -> Bool
vkWrongToken xs = not (length xs == 85 && foldl f True xs)
    where  f = (\acc x -> (isDigit x || isLower x) && acc)
