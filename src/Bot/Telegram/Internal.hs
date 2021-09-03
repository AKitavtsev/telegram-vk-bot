{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Internal where

import Data.Maybe (fromMaybe, isJust, isNothing)
import Network.HTTP.Simple

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M
import qualified Data.Text as T

import Bot
import Bot.Telegram.Types
import Dictionary
import Services.Config

forCopy :: [UPD] -> Config -> MapInt -> [UPD]
forCopy upds conf dict =
  map Tl $ concatMap repeating (filtred $ listUpdWithMessage upds)
  where
    filtred = filter (\(Tl x) -> txt x /= "/repeat" && txt x /= "/help")
    repeating ~(Tl x) = replicate (numRepeat x) x
    numRepeat x = M.findWithDefault (сonfigNumberRepeat conf) (usId x) dict

forHelp, forKb, listUpdWithKey, listUpdWithMessage :: [UPD] -> [UPD]
listUpdWithMessage = filter (\(Tl x) -> isJust (message x))

forHelp xs = filter (\(Tl x) -> txt x == "/help") $ listUpdWithMessage xs

forKb xs = filter (\(Tl x) -> txt x == "/repeat") $ listUpdWithMessage xs

listUpdWithKey = filter (\(Tl x) -> isJust (callback_query x))

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

usId :: Update -> Int
usId x = user_id us
  where
    us =
      if isNothing (message x)
        then cq_from (fromJust $ callback_query x)
        else fromJust (from $ fromJust $ message x)

mesId :: Update -> Int
mesId x = message_id (fromJust $ message x)

txt :: Update -> T.Text
txt x = fromMaybe "" (text (fromJust $ message x))

cbData :: Update -> String
cbData x = fromMaybe "" (cq_data (fromJust $ callback_query x))

getUserAndNumRep :: [UPD] -> [(Int, Int)]
getUserAndNumRep = map fgets
  where
    fgets ~(Tl x) = ((usId x), (read (cbData x) :: Int))
