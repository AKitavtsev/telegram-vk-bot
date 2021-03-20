{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VK (newHandle)  
  where

import Control.Exception
import Control.Monad.State
import Data.Aeson
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
        , copyMessages         = copyMessagesVk
        , sendMessagesWithKb   = sendMessagesWithKbVK
        , sendMessagesWithHelp = sendMessagesWithHelpVK
        , newDict              = newDictVK  
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
            resEither <- try (httpLBS  $ eventBuildRequest sess conf ts)
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
          copyMessagesVk upds dict = mapM_ copyMessage $ forCopy upds conf dict
            where 
              copyMessage (VK x) =  do
                let event = getVk_ItemMessage x
                infM "-- VK.copyMessage" (" to user " ++ (show $ m_from_id event))
                httpLBS  $ echoBuildRequest conf event
          sendMessagesWithKbVK upds dict = mapM_ sendMessageWithKb $ forKb upds
            where
              sendMessageWithKb (VK x) = do
                let event = getVk_ItemMessage x
                infM "-- VK.sendMessageWithKb" (" to user " ++ (show $ m_from_id event))
                httpLBS  $ kbBuildRequest conf dict event
          sendMessagesWithHelpVK upds = mapM_ sendMessageWithHelp $ forHelp upds
            where
              sendMessageWithHelp (VK x) = do
                let event = getVk_ItemMessage x
                infM "-- helpMessage" (" to user " ++ (show $ m_from_id event))
                httpLBS  $ helpBuildRequest conf event
          newDictVK upds dict = execState ( mapM_ changeMapInt
                                          $ getUserAndNumRep
                                          $ listUpdWithKey upds) dict
messageOK :: (Response LBC.ByteString)
          -> Config
          -> Log.Handle
          -> IO (Response LBC.ByteString)        
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

