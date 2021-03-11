{-# LANGUAGE OverloadedStrings #-}

module VK.Internal where

import Network.HTTP.Simple

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

import Bot
import Config
import VK.Data
import MapR

forCopy :: [UPD] -> Config -> MapInt -> [UPD]
forCopy xs conf dict = map VK $ concat (map repeating (filtred xs))
    where
      filtred xs = filter (\(VK x) -> (not 
        (m_text (getVk_ItemMessage x) == "/repeat" ||
         m_text (getVk_ItemMessage x) == "/help")) && 
         m_payload (getVk_ItemMessage x) == Nothing) xs
      repeating (VK x) = take (numRepeat x) $ repeat x
      numRepeat x = M.findWithDefault (сonfigNumberRepeat conf)
                                      (m_from_id (getVk_ItemMessage x)) dict

forHelp, forKb, listUpdWithKey :: [UPD] -> [UPD]
forHelp xs = filter (\(VK x) -> (m_text (getVk_ItemMessage x) == "/help")) xs
forKb xs = filter (\(VK x) -> (m_text (getVk_ItemMessage x) == "/repeat")) xs 
listUpdWithKey xs = 
    filter (\(VK x) -> not (m_payload (getVk_ItemMessage x) == Nothing)) xs

initBuildRequest :: Config -> Request
initBuildRequest  conf = setRequestQueryString qi
                  $ parseRequest_ "https://api.vk.com/method/groups.getLongPollServer"
    where
      qi = [ ("group_id",       Just (BC.pack $ group_id conf))
           , ("access_token",   Just (BC.pack $ сonfigToken conf))
           , ("v",              Just "5.126")]

echoBuildRequest :: Config -> Vk_ItemMessage -> Request
echoBuildRequest conf event = 
   setRequestQueryString qi $ parseRequest_ appVK
      where
        qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
             , ("forward_messages", Just (BC.pack $ show (m_id event)))
             , ("random_id",        Just (BC.pack $ show (m_random_id event)))
             , ("access_token",     Just (BC.pack $ сonfigToken conf))
             , ("v",                Just "5.126")]                                                
getVk_ItemMessage :: Event -> Vk_ItemMessage
getVk_ItemMessage e = m_message $ e_object e
