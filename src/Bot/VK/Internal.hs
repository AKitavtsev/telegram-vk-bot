{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Internal where

import Network.HTTP.Simple

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M

import Bot
import Drop
import MapR
import Servises.Config
import Bot.VK.Data

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
initBuildRequest conf = setRequestQueryString qi
                  $ parseRequest_ "https://api.vk.com/method/groups.getLongPollServer"
    where
      qi = [ ("group_id",       Just (BC.pack $ group_id conf))
           , ("access_token",   Just (BC.pack $ сonfigToken conf))
           , ("v",              Just "5.126")]

eventBuildRequest :: Session -> Config -> String -> Request
eventBuildRequest sess conf ts = setRequestQueryString qi
                         $ parseRequest_ $ server sess
    where 
      qi = [ ("act",  Just "a_check")
           , ("key",  Just (BC.pack $ key sess))
           , ("ts",   Just (BC.pack ts))
           , ("wait", Just (BC.pack $ show (myTimeout conf)))]

echoBuildRequest :: Config -> Vk_ItemMessage -> Request
echoBuildRequest conf event = setRequestQueryString qi $ parseRequest_ appVK
    where
      qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
           , ("forward_messages", Just (BC.pack $ show (m_id event)))
           , ("random_id",        Just (BC.pack $ show (m_random_id event)))
           , ("access_token",     Just (BC.pack $ сonfigToken conf))
           , ("v",                Just "5.126")] 

kbBuildRequest ::Config -> MapInt -> Vk_ItemMessage -> Request
kbBuildRequest conf dict event = setRequestQueryString qi $ parseRequest_ appVK
    where
      qi = [ ("user_id",      Just (BC.pack $ show (m_from_id event)))
           , ("random_id",    Just (BC.pack $ show (m_random_id event))) 
           , ("message",      Just (BC.pack $ textForRepeat (m_from_id event)))
           , ("keyboard",     Just (BC.pack $ LBC.unpack $ encode myKeyboard))
           , ("access_token", Just (BC.pack $ сonfigToken conf))
           , ("v",            Just "5.126")]
      buttonsForMyKb = [ Button (Action "text" "1" "1") "primary"
                      , Button (Action "text" "2" "2") "primary"
                      , Button (Action "text" "3" "3") "primary"
                      , Button (Action "text" "4" "4") "primary"
                      , Button (Action "text" "5" "5") "primary"]
      myKeyboard =  Keyboard  [buttonsForMyKb] True False
      textForRepeat x = (show $ M.findWithDefault
                        (сonfigNumberRepeat conf) x dict) ++ (messageForRepeat conf) 

helpBuildRequest :: Config -> Vk_ItemMessage -> Request
helpBuildRequest conf event = setRequestQueryString qi $ parseRequest_ appVK
    where
      qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
           , ("random_id",        Just (BC.pack $ show (m_random_id event))) 
           , ("message",          Just (BC.pack $ messageForHelp conf))
           , ("access_token",     Just (BC.pack $ сonfigToken conf))
           , ("v",                Just "5.126")]             

getUserAndNumRep :: [UPD] -> [(Int, Int)]
getUserAndNumRep xs = map fgets xs 
    where
      fgets (VK x) = ((m_from_id $ getVk_ItemMessage x), (payload x))
      payload x = read $ fromJust $ m_payload $ getVk_ItemMessage x ::Int
      
getVk_ItemMessage :: Event -> Vk_ItemMessage
getVk_ItemMessage e = m_message $ e_object e
