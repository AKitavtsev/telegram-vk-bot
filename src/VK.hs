{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VK where

import Control.Exception
import Control.Monad.State
import Data.Aeson
-- import Network.HTTP.Client
import Network.HTTP.Simple
import System.Exit
import Control.Concurrent (threadDelay)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M

import Bot
import Drop
import Config
import VK.Data
import Log
import MapR
import VK.Internal


newHandle :: Config -> Log.Handle-> IO Bot.Handle
newHandle conf handl = do
    return $ Bot.Handle
        { config               = conf
        , handlerLog           = handl
        , initSession          = initSessionVK
        , getUpdates           = getUpdatesVk
        , copyMessages          = copyMessagesVk
        , sendMessWithKeyboard = sendMessWithKeyboardVk
        , helpMessage          = helpMessageVK
        , getUserAndNumRep     = getUserAndNumRepVk   
        }
        where
          logLevel = сonfigLogg conf
          debM = (debugM handl) logLevel
          infM = (infoM handl) logLevel
          warM = (warnM handl) logLevel
          errM = (errorM handl) logLevel
          initSessionVK handle = do
            let titleM = "-- VK.initSession" 
            resEither <- try (httpLBS  $ initBuildRequest conf)
                 -- :: IO (Either SomeException (Response LBC.ByteString))
            res <- testException resEither handle
            let rsc = getResponseStatusCode res
            when ( not (rsc == 200)) $ do
                errM titleM ("-- status code of response " ++ show rsc)
                exitFailure
            let sessionJons = getResponseBody res
            let sessionR = (decode sessionJons) :: Maybe SessionResponse
            case sessionR of
                (Just (Vk_Response x)) -> do
                    debM titleM
                         ("-- initialized session with parameters:\n" ++ (show x))
                    loopBot handle x M.empty $ ts x
                    return ()
                Nothing -> do 
                    errM titleM " -- Wrong vkToken or group_id"
                    exitFailure                    
          getUpdatesVk handle sess ts = do  
            let titleM  = "-- VK.getUpdates" 
                handlog = handlerLog handle
            resEither <- try (httpLBS  $ eventBuildRequest sess)
            res' <- testException resEither handle
            res  <- messageOK res' conf handlog  
            let answerMaybe = (decode $ getResponseBody res) :: Maybe Answer
            when (answerMaybe == Nothing) $ do
              warM titleM " -- requesting new values key and ts"
              (initSession handle) handle
            let answer = fromJust answerMaybe
            when (a_ts answer == Nothing) $ do
              warM titleM " -- requesting new values key and ts"
              (initSession handle) handle
            let upds = fromJust $ a_updates answer
                newts = fromJust $ a_ts answer
            debM titleM (" List of Updates received = \n " ++ (show upds))
            return (map VK upds, newts)
              where
                eventBuildRequest sess = setRequestQueryString qi
                                         $ parseRequest_ $ server sess
                qi = [ ("act",  Just "a_check")
                     , ("key",  Just (BC.pack $ key sess))
                     , ("ts",   Just (BC.pack ts))
                     , ("wait", Just (BC.pack $ show (myTimeout conf)))]
          -- forHelpVk xs = 
            -- filter (\(VK x) -> (m_text (getVk_ItemMessage x) == "/help")) xs
          -- forKbVk xs = 
            -- filter (\(VK x) -> (m_text (getVk_ItemMessage x) == "/repeat")) xs 
          -- listUpdWithKeyVk xs = 
            -- filter (\(VK x) -> not (m_payload (getVk_ItemMessage x) == Nothing)) xs
          copyMessagesVk upds dict = mapM_ copyMessage $ forCopy upds conf dict
            where 
              copyMessage (VK x) =  do
                let event = getVk_ItemMessage x
                infM "-- VK.copyMessage" (" to user " ++ (show $ m_from_id event))
                httpLBS  $ echoBuildRequest conf event
          sendMessWithKeyboardVk dict (VK x) = do
            infM "-- VK.sendMessWithKeyboard" (" to user " ++ (show $ m_from_id event))
            httpLBS  $ kbBuildRequest dict event
              where
                event = getVk_ItemMessage x
                kbBuildRequest dict event = setRequestQueryString qi
                                            $ parseRequest_ appVK 
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
                                  (сonfigNumberRepeat conf) x dict)
                                   ++ (messageForRepeat conf)
          helpMessageVK (VK x) = do
            infM "-- helpMessage" (" to user " ++ (show $ m_from_id event))
            httpLBS  $ helpBuildRequest event
              where
                event = getVk_ItemMessage x
                helpBuildRequest event = setRequestQueryString qi $ parseRequest_ appVK
                qi = [ ("user_id",          Just (BC.pack $ show (m_from_id event)))
                     , ("random_id",        Just (BC.pack $ show (m_random_id event))) 
                     , ("message",          Just (BC.pack $ messageForHelp conf))
                     , ("access_token",     Just (BC.pack $ сonfigToken conf))
                     , ("v",                Just "5.126")]             
          getUserAndNumRepVk xs = map fgets xs 
            where
              fgets (VK x) = ((m_from_id $ getVk_ItemMessage x), (payload x))
              payload x = read $ fromJust $ m_payload $ getVk_ItemMessage x ::Int  





               
messageOK res conf handl = do
            let rsc = getResponseStatusCode res
            when ( not (rsc == 200)) $ do
              (errorM handl) (сonfigLogg conf) "-- messageOK  "
                                       ("-- status code of response " ++ show rsc)
              exitFailure
            return res
                                        
testException :: (Either SomeException (Response LBC.ByteString))
              ->  Bot.Handle
              ->  IO (Response LBC.ByteString)
testException rese handle = do
    case rese of
        Right val -> return val
        Left ex   -> do 
            (errorM  $ handlerLog handle) (сonfigLogg $ config handle)
                "-- Connection Failure" "-- Trying to initialize the session"
            threadDelay 25000000
            (initSession handle) handle
            httpLBS defaultRequest            

