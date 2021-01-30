{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram 
    where
    
import Data.Aeson
import Network.HTTP.Simple
import Control.Monad.State

import Config
import DataTelegram
import MapR

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M
import qualified Data.Text as T


{- | Trying to get another Update. Processing. We go to the next. 
-- Parameters - сonfig and offset
-}
loopTelegram ::  Config -> MapInt -> Int -> IO ()
loopTelegram  conf dict offs = do
-- https://api.telegram.org/bot<token>/getUpdates
    listUpdJons <- fetchJSON conf "/getUpdates" [offset offs, timeout 5]
    let listUpd = upds $ updatesResponseFromJSON listUpdJons
    debugM (сonfigLogg conf) "-- loopTelegram" (" -- List of Updates received:\n" ++ show listUpd)
    let forKb = forKeyboard listUpd
    infoM (сonfigLogg conf) "-- loopTelegram" (" -- " ++ show (length forKb)  ++ 
                                               " responses with keyboard\n" )
    let forC = forCopy listUpd
    infoM (сonfigLogg conf) "-- loopTelegram" (" -- " ++ show (length forC) ++ 
                                               " returns to addressees\n")
    
-- повторяем принятые сообщения repeat (bot.conf) раз
-- let listUpds = concat (map ((take (сonfigNumberRepeat conf)).repeat) listUpd)


    -- let (a, newst) = runState stackManip st
    -- print a

    
    mapM_ copyMessage forC
    
    loopTelegram conf dict $ newoffs listUpd
      where
        copyMessage :: Update -> IO LBC.ByteString
        copyMessage x =  fetchJSON conf "/copyMessage" [chatId userId,
                                                        fromChatId userId,
                                                        messageId (mesId x)]
          where userId = usId x
          
        forKeyboard xs = filter (\x -> (txt x) == "/repeat") xs
        forCopy xs = concat (map  repeating (filtred xs))
          where filtred xxs = filter (\x ->  not ((txt x) == "/repeat")) xxs
                repeating x = take (numRepeat x) $ repeat x
                numRepeat x = M.findWithDefault (сonfigNumberRepeat conf) (usId x) dict 
        
        
 

 
path ::  Config -> String -> BC.ByteString
path conf meth = BC.pack $ сonfigToken conf ++ meth
                         
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
                                          
fetchJSON :: Config -> String -> [QueryItem] -> IO LBC.ByteString
fetchJSON conf meth qi = do
    res <- httpLBS  $ buildRequest conf meth qi
    return (getResponseBody res)
            
buildRequest :: Config -> String -> [QueryItem] -> Request
buildRequest conf p querys = setRequestHost appTelegram
                        $ setRequestPath (path conf p)
                        $ setRequestQueryString querys
                        $ defaultRequest

updatesResponseFromJSON :: LBC.ByteString -> Maybe UpdatesResponse
updatesResponseFromJSON = decode
                                



    

    



    

