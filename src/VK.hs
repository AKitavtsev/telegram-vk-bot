{-# LANGUAGE OverloadedStrings #-}

module VK where

import Control.Monad.State
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Simple
import System.Exit

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M

import Config
import DataVK
import MapR

lVK = "-- loopVK "

initSession :: Config -> IO ()
initSession conf = do
    res <- httpLBS  $ initBuildRequest conf
    let rsc = getResponseStatusCode res   
    when ( not (rsc == 200)) $ do
        errorM (сonfigLogg conf) "-- initSession  "
                                       ("-- status code of response " ++ show rsc)
        exitFailure
    let sessionJons = getResponseBody res
    let sessionR = (decode sessionJons) :: Maybe SessionResponse
    case sessionR of
        (Just (Vk_Response x)) -> do
            infoM (сonfigLogg conf) "initialized session with parameters:\n"
                                    $ show x
            loopVk conf M.empty (ts x) x
        Nothing -> do 
            errorM (сonfigLogg conf) "-- initSession"
                                     " -- Wrong vkToken or group_id"
            exitFailure 

loopVk :: Config -> MapInt -> String -> Session -> IO () 
loopVk conf dict ts sess = do
    debugM (сonfigLogg conf) lVK ("ts = " ++ ts ++ "  dict = " ++ show dict)
    res' <- httpLBS  $ eventBuildRequest sess conf ts
    res <- messageOK res'
    
    let answerMaybe = (decode $ getResponseBody res) :: Maybe Answer
    when (answerMaybe == Nothing) $ do
        warnM (сonfigLogg conf) lVK " -- requesting new values key and ts"
        initSession conf
    -- let answer = case answerMaybe of Just x  -> x
    let answer = fromJust answerMaybe
    when (a_ts answer == Nothing) $ do
        warnM (сonfigLogg conf) lVK " -- requesting new values key and ts"
        initSession conf
    -- let newts = case a_ts answer of Just x -> x
    -- let newts =  fromJust $ a_ts answer
    -- let events = case a_updates answer of Just x -> x    
    -- let events = fromJust $ a_updates answer    
    debugM (сonfigLogg conf) lVK
          (" -- List of Updates received:\n" ++ show (fromJust $ a_updates answer))
    let fc = forCopy (a_updates answer) conf dict
    mapM_ copyMessage fc
    infoM (сonfigLogg conf) lVK (" -- " ++ show (length fc) ++ " returns to addressees")          
    let fkb = forKb (a_updates answer)
    mapM_ sendMessageWithKeyboard fkb
    infoM (сonfigLogg conf) lVK (" -- " ++ show (length fkb)  ++
                                 " requests sent to change the number of retries")
    let fh = forHelp (a_updates answer) 
    mapM_ helpMessage fh
    infoM (сonfigLogg conf) lVK (" -- " ++ show (length fh) ++ " help")
    let lp = listUpdWithPayload (a_updates answer)
    let newdict = execState (mapChangeMapInt $ getUsidAndPayload lp) dict    
    infoM (сonfigLogg conf) lVK (" -- " ++ show (length lp) ++ 
                                 " change the number of retries")
    loopVk conf newdict (fromJust $ a_ts answer) sess
      where
        -- copyMessage x =  do
            -- messageOK (httpLBS  $ echoBuildRequest conf (getVk_ItemMessage x))
        -- sendMessageWithKeyboard x = do
            -- messageOK (httpLBS  $ kbBuildRequest conf dict (getVk_ItemMessage x))
        -- helpMessage x =  do
            -- messageOK (httpLBS  $ helpBuildRequest conf (getVk_ItemMessage x))
            
        copyMessage x =  do
            res <- httpLBS  $ echoBuildRequest conf (getVk_ItemMessage x)
            messageOK res
        sendMessageWithKeyboard x = do
            res <- httpLBS  $ kbBuildRequest conf dict (getVk_ItemMessage x)
            messageOK res
        helpMessage x =  do
            res <- httpLBS  $ helpBuildRequest conf (getVk_ItemMessage x)
            messageOK res
        messageOK res = do
            let rsc = getResponseStatusCode res
            when ( not (rsc == 200)) $ do
              errorM (сonfigLogg conf) "-- messageOK  "
                                       ("-- status code of response " ++ show rsc)
              exitFailure
            return res
                                  
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

initBuildRequest :: Config -> Request
initBuildRequest  conf = setRequestQueryString qi
      $ parseRequest_ "https://api.vk.com/method/groups.getLongPollServer"
    where
      qi = [ ("group_id",       Just (BC.pack $ group_id conf))
           , ("access_token",   Just (BC.pack $ сonfigToken conf))
           , ("v",              Just "5.126")]
          
eventBuildRequest :: Session -> Config -> String -> Request
eventBuildRequest sess conf ts = setRequestQueryString qi $ parseRequest_ $ server sess
    where
      qi = [ ("act",  Just "a_check")
           , ("key",  Just (BC.pack $ key sess))
           , ("ts",   Just (BC.pack ts))
           , ("wait", Just (BC.pack $ show (myTimeout conf)))]

echoBuildRequest :: Config -> Vk_ItemMessage -> Request
echoBuildRequest conf event = setRequestQueryString qi $ parseRequest_  appVK
    where
      qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
           , ("forward_messages", Just (BC.pack $ show (m_id event)))
           , ("random_id",        Just (BC.pack $ show (m_random_id event)))
           , ("access_token",     Just (BC.pack $ сonfigToken conf))
           , ("v",                Just "5.126")]

helpBuildRequest :: Config -> Vk_ItemMessage -> Request
helpBuildRequest conf event = setRequestQueryString qi $ parseRequest_ appVK
    where
      qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
           , ("random_id",        Just (BC.pack $ show (m_random_id event))) 
           , ("message",          Just (BC.pack $ messageForHelp conf))
           , ("access_token",     Just (BC.pack $ сonfigToken conf))
           , ("v",                Just "5.126")]             

kbBuildRequest :: Config -> MapInt-> Vk_ItemMessage -> Request
kbBuildRequest conf dict event = setRequestQueryString qi
                     $ parseRequest_ appVK 
    where
      qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
           , ("random_id",        Just (BC.pack $ show (m_random_id event))) 
           , ("message",          Just (BC.pack $ textForRepeat (m_from_id event)))
           , ("keyboard",         Just (BC.pack $ LBC.unpack $ encode myKeyboard))
           , ("access_token",     Just (BC.pack $ сonfigToken conf))
           , ("v",                Just "5.126")]
      buttonsForMyKb = [ Button (Action "text" "1" "1") "primary"
                       , Button (Action "text" "2" "2") "primary"
                       , Button (Action "text" "3" "3") "primary"
                       , Button (Action "text" "4" "4") "primary"
                       , Button (Action "text" "5" "5") "primary"]
      myKeyboard =  Keyboard  [buttonsForMyKb] True False
      textForRepeat x = (show $ M.findWithDefault
                        (сonfigNumberRepeat conf) x dict)
                         ++ (messageForRepeat conf)

getUsidAndPayload :: [Event] -> [(Int, Int)]
getUsidAndPayload xs = map fgets xs 
    where 
      fgets x = ((m_from_id (getVk_ItemMessage x)), (payload x))
      -- payload x = case m_payload (getVk_ItemMessage x) of
                                -- Just y -> read y :: Int
      payload x = read $ fromJust $ m_payload $ getVk_ItemMessage x ::Int
                      