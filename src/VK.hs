{-# LANGUAGE OverloadedStrings #-}

module VK where

import Data.Aeson
import Network.HTTP.Simple

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M

import Config
import DataVK
import MapR


initSession :: Config -> IO ()
initSession conf = do
    sessionJons <- initFetchJSON conf
-- https://api.vk.com/method/groups.getLongPollServer?group_id=202471735&access_token=d8322e313e263070363ded0d53df2221e74c3e85edf044c101f543d8c5361bb564a0d41e23a621e3d5f3e&v=5.126
    let sessionR = sessionResponseFromJSON sessionJons
    case sessionR of
        (Just (Vk_Response x)) -> do
            infoM (сonfigLogg conf) "initialized session with parameters:\n"
                                    $ show x
            loopVk conf M.empty (ts x) x
        Nothing -> do 
            errorM (сonfigLogg conf) "-- initSession"
                                     " -- Wrong vkToken or group_id"
            error ""        
      
loopVk :: Config -> MapInt -> String -> Session -> IO () 
loopVk conf dict ts sess = do
    debugM (сonfigLogg conf) "--loopVK " ("ts = " ++ ts)
    eventsJSON <- eventFetchJSON sess conf ts
    -- let events = (eitherDecode eventsJSON) :: Either String Answer
    let events = (decode eventsJSON) :: Maybe Answer
    print eventsJSON
    print events
    let newts = case events of
                 Just x  -> a_ts x   
    loopVk conf dict newts sess
    
    error "So Long" 

initFetchJSON :: Config -> IO LBC.ByteString
initFetchJSON conf = do
    res <- httpLBS  $ initBuildRequest conf
    return (getResponseBody res)

initBuildRequest :: Config -> Request
initBuildRequest conf = setRequestQueryString qi
                      $ parseRequest_  
                        "https://api.vk.com/method/groups.getLongPollServer"
      where
        qi = [ ("group_id",  Just (BC.pack $ group_id conf))
             , ("access_token",  Just (BC.pack $ сonfigToken conf))
             , ("v",   Just "5.126")
             ]

eventFetchJSON :: Session -> Config -> String -> IO LBC.ByteString
eventFetchJSON sess conf ts = do
    res <- httpLBS  $ eventBuildRequest sess conf ts
    return (getResponseBody res)

eventBuildRequest :: Session -> Config -> String -> Request
eventBuildRequest sess conf ts = setRequestQueryString qi
                            $ parseRequest_ $ server sess
    where
        qi = [ ("act",  Just "a_check")
             , ("key",  Just (BC.pack $ key sess))
             , ("ts",   Just (BC.pack ts))
             , ("wait", Just (BC.pack $ show (myTimeout conf)))
             ]
    
-- echoFetchJSON :: Config -> IO LBC.ByteString
-- echoFetchJSON conf = do
    -- res <- httpLBS  $ echoBuildRequest conf
    -- return (getResponseBody res)

echoBuildRequest :: Config -> Request
echoBuildRequest conf = setRequestQueryString qi
                      $ parseRequest_  
                        "https://api.vk.com/method/messages.send"
      where
        qi = [ ("user_id",  Just ( ))
             , ("forward_messages", Just ( ))  
             , ("access_token",  Just (BC.pack $ сonfigToken conf))
             , ("v",   Just "5.126")
             ]

               

             
             
                          
sessionResponseFromJSON :: LBC.ByteString -> Maybe SessionResponse
sessionResponseFromJSON = decode