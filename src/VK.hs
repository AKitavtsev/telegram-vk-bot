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
    let sessionR = (decode sessionJons) :: Maybe SessionResponse
    -- let sessionR = sessionResponseFromJSON sessionJons
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
    debugM (сonfigLogg conf) "--loopVK " ("ts = " ++ ts ++ 
                                          "  dict = " ++ show dict)
    answerJSON <- eventFetchJSON sess conf ts
    -- let events = (eitherDecode eventsJSON) :: Either String Answer
    let answerMaybe = (decode answerJSON) :: Maybe Answer
        answer = case answerMaybe of Just x  -> x
        events = a_updates answer
        forC  = forCopy events
    debugM (сonfigLogg conf) "-- loopVK "
          (" -- List of Updates received:\n" ++ show events)
    mapM_ copyMessage forC
    infoM (сonfigLogg conf) "-- loopTelegram" (" -- " ++ show (length forC) ++ 
                                               " returns to addressees")   
    loopVk conf dict (a_ts answer) sess
      where
        copyMessage x =  echoFetchJSON (getVk_ItemMessage x) conf
        forCopy xs = concat (map  repeating (filtred xs))
          where 
            filtred xxs = filter (\x -> not 
                (m_text (getVk_ItemMessage x) == "/repeat" ||
                 m_text (getVk_ItemMessage x)== "/help" )) xxs
            repeating x = take (numRepeat x) $ repeat x
            numRepeat x = M.findWithDefault (сonfigNumberRepeat conf)
                                            (m_from_id (getVk_ItemMessage x))
                                            dict

getVk_ItemMessage :: Event -> Vk_ItemMessage
getVk_ItemMessage e = m_message $ e_object e

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
    
echoFetchJSON :: Vk_ItemMessage -> Config -> IO LBC.ByteString
echoFetchJSON event conf = do
    res <- httpLBS  $ echoBuildRequest event conf
    return (getResponseBody res)

echoBuildRequest :: Vk_ItemMessage -> Config -> Request
echoBuildRequest event conf = setRequestQueryString qi
                      $ parseRequest_  
                        "https://api.vk.com/method/messages.send"
      where
        qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
             , ("forward_messages", Just (BC.pack $ show (m_id event)))
             , ("random_id",        Just (BC.pack $ show (m_random_id event)))    
             , ("access_token",     Just (BC.pack $ сonfigToken conf))
             , ("v",                Just "5.126")
             ]
