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
    sessionJons <- initFetchJSON "/groups.getLongPollServer" [group_id, access_token conf, version]
-- https://api.vk.com/method/groups.getLongPollServer?group_id=202471735&access_token=d8322e313e263070363ded0d53df2221e74c3e85edf044c101f543d8c5361bb564a0d41e23a621e3d5f3e&v=5.126
    let sessionR = sessionResponseFromJSON sessionJons
    case sessionResponseFromJSON sessionJons of
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
    events <- eventFetchJSON sess conf
    print events
    error "So long"
    -- loopVk conf dict ts sess
    
vk_path ::  String -> BC.ByteString
vk_path meth = BC.pack $ "method" ++ meth

access_token :: Config -> QueryItem
access_token conf = ("access_token", Just $ BC.pack (сonfigToken conf))

version :: QueryItem
version = ("v", Just $ BC.pack "5.126")

-- group_id, по всей видимости, надо в конфиг
group_id :: QueryItem
group_id = ("group_id", Just $ BC.pack "202471735")

initFetchJSON :: String -> [QueryItem] -> IO LBC.ByteString
initFetchJSON meth qi = do
    res <- httpLBS  $ initBuildRequest meth qi
    return (getResponseBody res)
               
initBuildRequest :: String -> [QueryItem] -> Request
initBuildRequest meth qi = setRequestHost appVK
                         $ setRequestPath (vk_path meth)
                         $ setRequestQueryString qi
                         $ defaultRequest
                         
eventFetchJSON :: Session -> Config -> IO LBC.ByteString
eventFetchJSON sess conf = do
    res <- httpLBS  $ eventBuildRequest sess conf
    return (getResponseBody res)
               
eventBuildRequest :: Session -> Config -> Request
eventBuildRequest sess conf = setRequestQueryString qi
                            $ parseRequest_ $ server sess
    where
        qi = [ ("act",  Just "a_check")
             , ("key",  Just (BC.pack $ key sess))
             , ("ts",   Just (BC.pack $ ts sess))
             , ("wait", Just (BC.pack $ show (myTimeout conf)))
             ]
                          
sessionResponseFromJSON :: LBC.ByteString -> Maybe SessionResponse
sessionResponseFromJSON = decode