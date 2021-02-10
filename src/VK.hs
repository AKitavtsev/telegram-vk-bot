{-# LANGUAGE OverloadedStrings #-}

module VK where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import           Data.Aeson

import Config
import DataVK
import MapR


initSession :: Config -> IO ()
initSession conf = do
    session <- vk_fetchJSON "/groups.getLongPollServer" [group_id, access_token conf, version]
    print session
    error "bye bye"

-- https://api.vk.com/method/groups.getLongPollServer?group_id=202471735&access_token=d8322e313e263070363ded0d53df2221e74c3e85edf044c101f543d8c5361bb564a0d41e23a621e3d5f3e&v=5.126

loopVk :: IO () 
loopVk= do 
    print "Bot vk in development"

vk_path ::  String -> BC.ByteString
vk_path meth = BC.pack $ "method" ++ meth

access_token :: Config -> QueryItem
access_token conf = ("access_token", Just $ BC.pack (сonfigToken conf))

version :: QueryItem
version = ("v", Just $ BC.pack "5.126")

-- group_id, по всей видимости надо в конфиг
group_id :: QueryItem
group_id = ("group_id", Just $ BC.pack "202471735")

vk_fetchJSON :: String -> [QueryItem] -> IO LBC.ByteString
vk_fetchJSON meth qi = do
    res <- httpLBS  $ vk_buildRequest meth qi
    return (getResponseBody res)
            
vk_buildRequest :: String -> [QueryItem] -> Request
vk_buildRequest meth qi = setRequestHost appVK
                        $ setRequestPath (vk_path meth)
                        $ setRequestQueryString qi
                        $ defaultRequest