{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram 
    (loopTelegram
    ) where
    
import Data.Aeson
import Network.HTTP.Simple

import Config
import DataTelegram

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Text as T


{- | Trying to get another Update. Processing. We go to the next. 
-- Parameters - сonfig and offset
-}
loopTelegram ::  Config -> Int -> IO ()
loopTelegram  conf offs = do
-- https://api.telegram.org/bot<token>/getUpdates
    listUpdJons <- fetchJSON "/getUpdates" [offset offs, timeout 5]
    let listUpd = upds $ updatesResponseFromJSON listUpdJons
    
    
    
-- повторяем принятые сообщения repeat (bot.conf) раз
    let listUpds = concat (map ((take (сonfigNumberRepeat conf)).repeat) listUpd)
    
    mapM_ copyMessage listUpds
    loopTelegram conf $ newoffs listUpd
      where
        copyMessage :: Update -> IO LBC.ByteString
        copyMessage x =  fetchJSON "/copyMessage" [chatId userId,
                                                   fromChatId userId,
                                                             messageId (mesId x)]
          where userId = usId x
                         
        newoffs :: [Update] -> Int
        newoffs [] = 0
        newoffs x = upId (last x) + 1

        chatId :: Int -> QueryItem
        chatId chid = ("chat_id", Just $ BC.pack (show chid))

        fromChatId :: Int -> QueryItem
        fromChatId chid = ("from_chat_id", Just $ BC.pack (show chid))

        messageId :: Int -> QueryItem
        messageId mid = ("message_id", Just $ BC.pack (show mid))

        offset :: Int -> QueryItem
        offset os = ("offset", Just $ BC.pack (show os))

        timeout :: Int -> QueryItem
        timeout to = ("timeout", Just $ BC.pack (show to))

        upds :: Maybe UpdatesResponse -> [Update]
        upds (Just (Response x)) = x
        upds Nothing = [] 

        usId :: Update  -> Int
        usId x = let us = case message x of
                            Just y -> from y
                 in case us of
                     Just z -> user_id z
                     
        upId :: Update -> Int
        upId x = update_id x

        mesId :: Update -> Int
        mesId x = case message x of
                     Just y -> message_id y
                     
        txt :: Update -> T.Text
        txt x = let t = case message x of
                     Just y -> text y
                in case t of
                    Just z -> z
                    Nothing -> ""
                            

               
        fetchJSON :: String -> [QueryItem] -> IO LBC.ByteString
        fetchJSON meth qi = do
            res <- httpLBS  $ buildRequest meth qi
            return (getResponseBody res)
            
        buildRequest :: String -> [QueryItem] -> Request
        buildRequest p querys = setRequestHost appTelegram
                                $ setRequestPath (path p)
                                $ setRequestQueryString querys
                                $ defaultRequest

        updatesResponseFromJSON :: LBC.ByteString -> Maybe UpdatesResponse
        updatesResponseFromJSON = decode
                                
        path ::  String -> BC.ByteString
        path meth = BC.pack $ сonfigToken conf ++ meth


    

    



    

