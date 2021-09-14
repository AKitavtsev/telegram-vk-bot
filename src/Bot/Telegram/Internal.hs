{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Internal where

import Data.Maybe (isJust, isNothing, fromMaybe)
import Network.HTTP.Simple

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M
import qualified Data.Text as T

import Bot
import Bot.Telegram.Types
import Services.Config

forCopy :: [Update] -> Config -> MapInt -> [Update]
forCopy upds conf dict =
  concatMap repeating (filtred $ listUpdWithMessage upds)
  where
    filtred = filter (\x -> txt x /= "/repeat" && txt x /= "/help")
    repeating x = replicate (numRepeat x) x
    numRepeat x = M.findWithDefault (сonfigNumberRepeat conf) (usId x) dict

forHelp, forKb, listUpdWithMessage :: [Update] -> [Update]
listUpdWithMessage = filter (\x -> isJust (message x))

forHelp xs = filter (\x -> txt x == "/help") $ listUpdWithMessage xs

forKb xs = filter (\x -> txt x == "/repeat") $ listUpdWithMessage xs

eventBuildRequest :: Config -> String -> Request
eventBuildRequest conf offs =
  setRequestHost appTelegram $
  setRequestPath (BC.pack $ сonfigToken conf ++ "/getUpdates") $
  setRequestQueryString qi defaultRequest
  where
    qi =
      [ ("offset", Just $ BC.pack offs)
      , ("timeout", Just $ BC.pack (show $ myTimeout conf))
      ]

echoBuildRequest :: Config -> Update -> Request
echoBuildRequest conf upd =
  setRequestHost appTelegram $
  setRequestPath (BC.pack $ сonfigToken conf ++ "/copyMessage") $
  setRequestQueryString qi defaultRequest
  where
    qi =
      [ ("chat_id", Just $ BC.pack (show $ usId upd))
      , ("from_chat_id", Just $ BC.pack (show $ usId upd))
      , ("message_id", Just $ BC.pack (show $ mesId upd))
      ]

kbBuildRequest :: Config -> MapInt -> Update -> Request
kbBuildRequest conf dict upd =
  setRequestHost appTelegram $
  setRequestPath (BC.pack $ сonfigToken conf ++ "/sendMessage") $
  setRequestQueryString qi defaultRequest
  where
    qi =
      [ ("chat_id", Just $ BC.pack (show $ usId upd))
      , ("text", Just $ BC.pack $ textForRepeat $ usId upd)
      , ("reply_markup", Just $ BC.pack $ LBC.unpack $ encode myKeyboard)
      ]
    buttonsForMyKb =
      [ InlineKeyboardButton "1" (Just "1")
      , InlineKeyboardButton "2" (Just "2")
      , InlineKeyboardButton "3" (Just "3")
      , InlineKeyboardButton "4" (Just "4")
      , InlineKeyboardButton "5" (Just "5")
      ]
    myKeyboard = InlineKeyboardMarkup [buttonsForMyKb]
    textForRepeat x =
      show (M.findWithDefault (сonfigNumberRepeat conf) x dict) ++
      messageForRepeat conf

helpBuildRequest :: Config -> Update -> Request
helpBuildRequest conf upd =
  setRequestHost appTelegram $
  setRequestPath (BC.pack $ сonfigToken conf ++ "/sendMessage") $
  setRequestQueryString qi defaultRequest
  where
    qi =
      [ ("chat_id", Just $ BC.pack (show $ usId upd))
      , ("text", Just $ BC.pack $ messageForHelp conf)
      ]

newoffs :: [Update] -> String
newoffs [] = "0"
newoffs x = show $ update_id (last x) + 1

listUpd :: Maybe UpdatesResponse -> [Update]
listUpd (Just (TlResponse x)) = x
listUpd Nothing = []

