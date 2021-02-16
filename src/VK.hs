{-# LANGUAGE OverloadedStrings #-}

module VK where

import Control.Monad.State
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
        where
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
      
loopVk :: Config -> MapInt -> String -> Session -> IO () 
loopVk conf dict ts sess = do
    debugM (сonfigLogg conf) "-- loopVK " ("ts = " ++ ts ++ 
                                          "  dict = " ++ show dict)
    answerJSON <- eventFetchJSON sess conf ts
    -- let events = (eitherDecode eventsJSON) :: Either String Answer
    -- print answerJSON
    let answerMaybe = (decode answerJSON) :: Maybe Answer
        answer = case answerMaybe of Just x  -> x
        events = a_updates answer
        listUpdWithPayload = 
            filter (\x -> not (m_payload  (getVk_ItemMessage x) == Nothing)) events
        forKb = filter (\x -> (m_text (getVk_ItemMessage x) == "/repeat")) events 
        forC  = forCopy events
        forHelp = filter (\x -> (m_text (getVk_ItemMessage x) == "/help")) events
    debugM (сonfigLogg conf) "-- loopVK "
          (" -- List of Updates received:\n" ++ show events)
    mapM_ copyMessage forC
    mapM_ sendMessageWithKeyboard forKb
    -- mapM_ sendMessageHelp forHelp
    
    infoM (сonfigLogg conf) "-- loopVk"
                                    (" -- " ++ show (length forKb)  ++
                                     " requests sent to change the number of retries")
    infoM (сonfigLogg conf) "-- loopVk" (" -- " ++ show (length forC) ++ 
                                               " returns to addressees")
    infoM (сonfigLogg conf) "-- loopVk"
                                    (" -- " ++ show (length forHelp) ++ " help")
    infoM (сonfigLogg conf) "-- loopVk"
                                    (" -- " ++ show (length listUpdWithPayload) ++ 
                                     " change the number of retries")
    let newdict = execState (mapChangeMapInt  
                             $ getUsidAndPayload listUpdWithPayload) dict
    loopVk conf newdict (a_ts answer) sess
      where
        copyMessage x =  echoFetchJSON (getVk_ItemMessage x) conf
        forCopy xs = concat (map  repeating (filtred xs))
          where 
            filtred xxs = filter (\x -> (not 
                (m_text (getVk_ItemMessage x) == "/repeat" ||
                 m_text (getVk_ItemMessage x) == "/help")) && 
                 m_payload (getVk_ItemMessage x) == Nothing) xxs
            repeating x = take (numRepeat x) $ repeat x
            numRepeat x = M.findWithDefault (сonfigNumberRepeat conf)
                                            (m_from_id (getVk_ItemMessage x))
                                            dict
        sendMessageWithKeyboard x =  kbFetchJSON (getVk_ItemMessage x) conf

        getUsidAndPayload xs = map fgets xs 
           where fgets x = ((m_from_id (getVk_ItemMessage x)), (payload x))
                 payload x = case m_payload (getVk_ItemMessage x) of
                                Just y -> read y :: Int
                                     
        getVk_ItemMessage :: Event -> Vk_ItemMessage
        getVk_ItemMessage e = m_message $ e_object e


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
                     
        kbFetchJSON :: Vk_ItemMessage -> Config -> IO LBC.ByteString
        kbFetchJSON event conf = do
            res <- httpLBS  $ kbBuildRequest event conf
            return (getResponseBody res)

        kbBuildRequest :: Vk_ItemMessage -> Config -> Request
        kbBuildRequest event conf = setRequestQueryString qi
                              $ parseRequest_  
                                "https://api.vk.com/method/messages.send"
              where
                qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
                     , ("random_id",        Just (BC.pack $ show (m_random_id event))) 
                     , ("message",          Just (BC.pack $ textForRepeat 
                                                    (m_from_id event)))
                     , ("keyboard",         Just (BC.pack $ LBC.unpack $ encode myKeyboard))
                     , ("access_token",     Just (BC.pack $ сonfigToken conf))
                     , ("v",                Just "5.126")
                     ]
                buttonsForMyKb :: [Button]
                buttonsForMyKb = [
                    Button { action = Action {a_type = "text", a_label = "1", a_payload = "1"}
                           , color = "primary"}
                  , Button { action = Action {a_type = "text", a_label = "2", a_payload = "2"}
                           , color = "primary"}
                  , Button { action = Action {a_type = "text", a_label = "3", a_payload = "3"}
                           , color = "primary"}
                  , Button { action = Action {a_type = "text", a_label = "4", a_payload = "4"}
                           , color = "primary"}
                  , Button { action = Action {a_type = "text", a_label = "5", a_payload = "5"}
                           , color = "primary"}
                  ]
                myKeyboard :: Keyboard
                myKeyboard =  Keyboard { buttons = [buttonsForMyKb]
                                       , inline = True}
                                       
        textForRepeat x = (show $ M.findWithDefault
                          (сonfigNumberRepeat conf)
                          x dict) ++ (messageForRepeat conf)                       
                                       