module Config where

import Control.Monad (when)

import Data.Char (isDigit, isLower)
import System.Exit (exitFailure)

import Services.Types

import qualified Data.Configurator as C
import qualified Data.Text as T

data Config =
  Config
    { levelLog :: !Priority
    , сonfigApi :: !String
    , groupId :: !String
    , сonfigToken :: !String
    , сonfigNumberRepeat :: !Int
    , messageForRepeat :: !String
    , messageForHelp :: !String
    , myTimeout :: !Int
    }
  deriving (Show)


getConfig = do
      conf <- C.load [C.Optional "bot.conf", C.Optional "local_bot.conf"]
      levelStr <-
        C.lookupDefault "INFO" conf (T.pack "logger.loggLevel") :: IO String
      let level =
            case levelStr of
              "DEBUG" -> DEBUG
              "WARN" -> WARN
              "INFO" -> INFO
              "ERROR" -> ERROR
              _ -> INFO
      apiStr <- 
        C.lookupDefault "telegram" conf (T.pack "bot.api") :: IO String
      let api =
            case apiStr of
              "telegram" -> TELEGRAM
              "vk" -> VK
              _ -> TELEGRAM     
           
      groupId <- C.lookupDefault "" conf (T.pack "bot.groupId") :: IO String
      token <- getToken conf api
      numberRepeat <- C.lookupDefault 1 conf (T.pack "bot.repeat") :: IO Int
      messageForRepeat <-
        C.lookupDefault "" conf (T.pack "bot.messageForRepeat") :: IO String
      messageForHelp <-
        C.lookupDefault "" conf (T.pack "bot.messageForHelp") :: IO String
      myTimeout <- C.lookupDefault 5 conf (T.pack "bot.timeout") :: IO Int
      return
        (Config
           level
           api
           groupId
           token
           numberRepeat
           messageForRepeat
           messageForHelp
           myTimeout)
      where
        getToken conf TELEGRAM = do
          token <- C.lookupDefault "" conf (T.pack "bot.token") :: IO String
          when (wrongToken token) $ do
            putStrLn
              ("ERROR  token for telegram should look like:\n " ++
               "bot1509893058:AAD3uC_cmyxDQJfBZtQgs2E4-K55xivO8Wc")
            exitFailure
          return token
        getToken conf VK = do
          token <- C.lookupDefault "" conf (T.pack "bot.vkToken") :: IO String
          when (vkWrongToken token) $ do
            putStrLn
              ("ERROR  token for vk should look like:\n " ++               "f471666483f81526e052c193223df886e08x1de38a7b823d4614ec44f3z680ffce51f29f177d3s6be664y")
            exitFailure
          return token

wrongToken :: String -> Bool
wrongToken ('b':'o':'t':xs) = length xs /= 46
wrongToken _ = True

vkWrongToken :: String -> Bool
vkWrongToken xs = not (length xs == 85 && foldl f True xs)
  where
    f acc x = (isDigit x || isLower x) && acc
