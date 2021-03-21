{-# LANGUAGE OverloadedStrings #-}

module Telegram.Internal where

import Network.HTTP.Simple

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M
import qualified Data.Text as T

import Bot
import Config
import Telegram.Data
import MapR
import Drop

forCopy :: [UPD] -> Config -> MapInt -> [UPD]
forCopy xs conf dict = map Tl $ concat (map repeating (filtred $ listUpdWithMessage xs))
    where filtred xs = filter (\(Tl x) ->  not ((txt x) == "/repeat" ||
                                            (txt x) == "/help" )) xs
          repeating (Tl x) = take (numRepeat x) $ repeat x
          numRepeat x = M.findWithDefault (сonfigNumberRepeat conf) (usId x) dict 
                                      
forHelp, forKb, listUpdWithKey, listUpdWithMessage :: [UPD] -> [UPD]
listUpdWithMessage xs = filter (\(Tl x) -> not ((message x) == Nothing)) xs
forHelp xs = filter (\(Tl x) -> (txt x) == "/help") $ listUpdWithMessage xs
forKb xs = filter (\(Tl x) -> (txt x) == "/repeat") $ listUpdWithMessage xs
listUpdWithKey xs = filter (\(Tl x) -> not ((callback_query x) == Nothing)) xs

eventBuildRequest :: Session -> Config -> String -> Request
eventBuildRequest sess conf offs = setRequestHost appTelegram
                       $ setRequestPath (BC.pack $ сonfigToken conf ++ "/getUpdates")
                       $ setRequestQueryString qi
                       $ defaultRequest
    where
      qi = [ ("offset",  Just $ BC.pack offs)
           , ("timeout", Just $ BC.pack (show $ myTimeout conf))]
           
echoBuildRequest :: Config -> Update -> Request
echoBuildRequest conf upd = setRequestHost appTelegram
                     $ setRequestPath (BC.pack $ сonfigToken conf ++ "/copyMessage")
                     $ setRequestQueryString qi
                     $ defaultRequest
    where
      qi = [ ("chat_id", Just $ BC.pack (show $ usId upd))
           , ("from_chat_id", Just $ BC.pack (show $ usId upd))
           , ("message_id", Just $ BC.pack (show $ mesId upd))]

kbBuildRequest :: Config -> MapInt-> Update -> Request
kbBuildRequest conf dict upd = setRequestHost appTelegram
                     $ setRequestPath (BC.pack $ сonfigToken conf ++ "/sendMessage")
                     $ setRequestQueryString qi
                     $ defaultRequest
    where
      qi = [ ("chat_id", Just $ BC.pack (show $ usId upd))
           , ("text", Just $ BC.pack $ textForRepeat $ usId upd)
           , ("reply_markup", Just $ BC.pack $ LBC.unpack $ encode myKeyboard)]
      buttonsForMyKb = [ InlineKeyboardButton "1" (Just "1")
                       , InlineKeyboardButton "2" (Just "2")
                       , InlineKeyboardButton "3" (Just "3")
                       , InlineKeyboardButton "4" (Just "4")
                       , InlineKeyboardButton "5" (Just "5")]
      myKeyboard =  InlineKeyboardMarkup [buttonsForMyKb]

      textForRepeat x = (show $ M.findWithDefault
                        (сonfigNumberRepeat conf) x dict)
                         ++ (messageForRepeat conf)

helpBuildRequest :: Config -> Update -> Request
helpBuildRequest conf upd = setRequestHost appTelegram
                     $ setRequestPath (BC.pack $ сonfigToken conf ++ "/sendMessage")
                     $ setRequestQueryString qi
                     $ defaultRequest
     where
      qi = [ ("chat_id", Just $ BC.pack (show $ usId upd))
           , ("text", Just $ BC.pack $ messageForHelp conf)]
                        
newoffs :: [Update] -> String
newoffs [] = "0"
newoffs x = show $ update_id (last x) + 1

listUpd :: Maybe UpdatesResponse -> [Update]
listUpd (Just (Tl_Response x)) = x
listUpd Nothing = []

usId :: Update  -> Int
usId x = let us = case message x of
                      Just y -> from y
                      Nothing -> case callback_query x of
                        Just y -> Just (cq_from  y)
         in case us of
                Just z -> user_id z

mesId :: Update -> Int
mesId x = case message x of
              Just y -> message_id y
              
txt :: Update -> T.Text
txt x = let t = case message x of
             Just y -> text y
        in case t of
            Just z -> z
            Nothing -> ""
                
cbData :: Update -> String
cbData x = let cq = case callback_query x of
                    Just y -> cq_data y
           in case cq of
            Just z -> z
            Nothing -> ""
 
getUserAndNumRep :: [UPD] -> [(Int, Int)]
getUserAndNumRep xs = map fgets xs
    where fgets (Tl x) = ((usId x), (read (cbData x)::Int)) 
      
