{-# LANGUAGE OverloadedStrings #-}

module VK where

import Control.Monad.State
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Simple

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M

import Config
import DataVK
import MapR


initSession :: Config -> IO ()
initSession conf = do
    sessionJons <- initFetchJSON
    let sessionR = (decode sessionJons) :: Maybe SessionResponse
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
            initFetchJSON :: IO LBC.ByteString
            initFetchJSON  = do
                res <- httpLBS  $ initBuildRequest
                return (getResponseBody res)

            initBuildRequest :: Request
            initBuildRequest  = setRequestQueryString qi
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
    -- answerJSON <- eventFetchJSON sess conf ts
    
    res <- httpLBS  $ eventBuildRequest sess conf ts
    -- let answerJSON = getResponseBody res
    let answerMaybe = (decode $ getResponseBody res) :: Maybe Answer
-- тут, вроде ошибка api : при {"failed":1,"ts":30} - ts - Numder , а при
-- {"ts":"4","updates":[{"type":................ - ts - String, по этому, 
-- следующий закомментированный код бессмысленный. Все равно до этого не дойдет
-- не распарсится на let answerMaybe = (decode answerJSON) :: Maybe Answer, 
-- поэтому делаем так:
    when (answerMaybe == Nothing) $ do
        warnM (сonfigLogg conf) "-- loopVk" " -- requesting new values key and ts"
        initSession conf
    let answer = case answerMaybe of Just x  -> x
    when (a_ts answer == Nothing) $ do
        warnM (сonfigLogg conf) "-- loopVk" " -- requesting new values key and ts"
        initSession conf
    let newts = case a_ts answer of Just x -> x
-- Вот надо было бы так, но из-за, см. комментарий выше 
    -- when (a_updates answer == Nothing) $ do
        -- warnM (сonfigLogg conf) "-- loopVk" 
                                -- (" -- history is lost, start with ts = " ++ newts)
        -- loopVk conf dict newts sess
        events = case a_updates answer of Just x -> x    
    debugM (сonfigLogg conf) "-- loopVK "
          (" -- List of Updates received:\n" ++ show events)
          
    mapM_ copyMessage $ forCopy (a_updates answer) conf dict
    mapM_ sendMessageWithKeyboard $ forKb (a_updates answer)
    mapM_ helpMessage $ forHelp (a_updates answer)
    let newdict = execState (mapChangeMapInt  
                             $ getUsidAndPayload 
                             $ listUpdWithPayload (a_updates answer))
                             dict
    
    -- infoM (сonfigLogg conf) "-- loopVk"
                                    -- (" -- " ++ show (length forKb)  ++
                                     -- " requests sent to change the number of retries")
    -- infoM (сonfigLogg conf) "-- loopVk" (" -- " ++ show (length forC) ++ 
                                         -- " returns to addressees")
    -- infoM (сonfigLogg conf) "-- loopVk"
                                    -- (" -- " ++ show (length forHelp) ++ " help")
    -- infoM (сonfigLogg conf) "-- loopVk"
                                    -- (" -- " ++ show (length listUpdWithPayload) ++ 
                                     -- " change the number of retries")
    loopVk conf newdict newts sess
      where
        copyMessage x =  echoFetchJSON (getVk_ItemMessage x)
        sendMessageWithKeyboard x =  kbFetchJSON (getVk_ItemMessage x)
        helpMessage x =  helpFetchJSON (getVk_ItemMessage x)
        getUsidAndPayload xs = map fgets xs 
           where fgets x = ((m_from_id (getVk_ItemMessage x)), (payload x))
                 payload x = case m_payload (getVk_ItemMessage x) of
                                Just y -> read y :: Int
                                     
        -- eventFetchJSON :: Session -> Config -> String -> IO LBC.ByteString
        -- eventFetchJSON sess conf ts = do
            -- res <- httpLBS  $ eventBuildRequest sess conf ts
            -- return (getResponseBody res)

        -- eventBuildRequest :: Session -> String -> Request
        -- eventBuildRequest sess ts = setRequestQueryString qi
                                    -- $ parseRequest_ $ server sess
            -- where
                -- qi = [ ("act",  Just "a_check")
                     -- , ("key",  Just (BC.pack $ key sess))
                     -- , ("ts",   Just (BC.pack ts))
                     -- , ("wait", Just (BC.pack $ show (myTimeout conf)))
                     -- ]
            
        echoFetchJSON :: Vk_ItemMessage -> IO LBC.ByteString
        echoFetchJSON event = do
            res <- httpLBS  $ echoBuildRequest event
            return (getResponseBody res)

        echoBuildRequest :: Vk_ItemMessage -> Request
        echoBuildRequest event = setRequestQueryString qi
                              $ parseRequest_  appVK
                                -- "https://api.vk.com/method/messages.send"
              where
                qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
                     , ("forward_messages", Just (BC.pack $ show (m_id event)))
                     , ("random_id",        Just (BC.pack $ show (m_random_id event)))
                     , ("access_token",     Just (BC.pack $ сonfigToken conf))
                     , ("v",                Just "5.126")
                     ]
        helpFetchJSON :: Vk_ItemMessage -> IO LBC.ByteString
        helpFetchJSON event = do
            res <- httpLBS  $ helpBuildRequest event
            return (getResponseBody res)

        helpBuildRequest :: Vk_ItemMessage -> Request
        helpBuildRequest event = setRequestQueryString qi
                              $ parseRequest_ appVK 
                                -- "https://api.vk.com/method/messages.send"
              where
                qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
                     , ("random_id",        Just (BC.pack $ show (m_random_id event))) 
                     , ("message",          Just (BC.pack $ messageForHelp conf))
                     , ("access_token",     Just (BC.pack $ сonfigToken conf))
                     , ("v",                Just "5.126")
                     ]             
        kbFetchJSON :: Vk_ItemMessage -> IO LBC.ByteString
        kbFetchJSON event = do
            res <- httpLBS  $ kbBuildRequest event
            return (getResponseBody res)

        kbBuildRequest :: Vk_ItemMessage -> Request
        kbBuildRequest event = setRequestQueryString qi
                              $ parseRequest_ appVK 
                                -- "https://api.vk.com/method/messages.send"
           where
             qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
                  , ("random_id",        Just (BC.pack $ show (m_random_id event))) 
                  , ("message",  Just (BC.pack $ textForRepeat (m_from_id event)))
                  , ("keyboard",  Just (BC.pack $ LBC.unpack $ encode myKeyboard))
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
                                       , inline = True
                                       , one_time = False}
                                       
        textForRepeat x = (show $ M.findWithDefault
                          (сonfigNumberRepeat conf) x dict)
                           ++ (messageForRepeat conf)                       

getVk_ItemMessage :: Event -> Vk_ItemMessage
getVk_ItemMessage e = m_message $ e_object e

listUpdWithPayload :: Maybe [Event] -> [Event]
listUpdWithPayload ~ (Just e) = 
    filter (\x -> not (m_payload  (getVk_ItemMessage x) == Nothing)) e

forKb :: Maybe [Event] -> [Event]
forKb ~ (Just e) = filter (\x -> (m_text (getVk_ItemMessage x) == "/repeat")) e 

forHelp :: Maybe [Event] -> [Event]
forHelp ~ (Just e) = filter (\x -> (m_text (getVk_ItemMessage x) == "/help")) e

forCopy:: Maybe [Event] -> Config -> MapInt -> [Event]
forCopy  ~ (Just e) c d = concat (map  repeating (filtred e))
    where 
      filtred xs = filter (\x -> (not 
                (m_text (getVk_ItemMessage x) == "/repeat" ||
                 m_text (getVk_ItemMessage x) == "/help")) && 
                 m_payload (getVk_ItemMessage x) == Nothing) xs
      repeating x = take (numRepeat x) $ repeat x
      numRepeat x = M.findWithDefault (сonfigNumberRepeat c)
                                      (m_from_id (getVk_ItemMessage x)) d
    
eventBuildRequest :: Session -> Config -> String -> Request
eventBuildRequest sess conf ts = setRequestQueryString qi $ parseRequest_ $ server sess
            where
                qi = [ ("act",  Just "a_check")
                     , ("key",  Just (BC.pack $ key sess))
                     , ("ts",   Just (BC.pack ts))
                     , ("wait", Just (BC.pack $ show (myTimeout conf)))
                     ]                                       