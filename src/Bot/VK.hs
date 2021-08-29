{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Bot.VK (newHandle)  
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
import MapR
import Servises.Config
import Bot.VK.Data
import Servises.Logger
import Bot.VK.Internal


newHandle :: Config -> IO Bot.Handle
newHandle conf = do
    return $ Bot.Handle
        { initSession          = initSessionVK
        , getUpdates           = getUpdatesVk
        , copyMessages         = copyMessagesVk
        , sendMessagesWithKb   = sendMessagesWithKbVK
        , sendMessagesWithHelp = sendMessagesWithHelpVK
        , newDict              = newDictVK  
        }
        where 
          initSessionVK handle hLogger = do            
            resEither <- try (httpLBS  $ initBuildRequest conf)
                 -- :: IO (Either SomeException (Response LBC.ByteString))
            res <- testException resEither handle hLogger
            let rsc = getResponseStatusCode res
            when ( not (rsc == 200)) $ do
                logError hLogger ("-- status code of response " ++ show rsc)
                exitFailure
            let sessionJson = getResponseBody res
            let sessionR = (decode sessionJson) :: Maybe SessionResponse
            case sessionR of
                (Just (Vk_Response x)) -> do
                    logDebug hLogger
                         ("-- initialized session with parameters:\n" ++ (show x)) 
                    loopBot handle hLogger (DataLoop x [] M.empty $ ts x)
                    return ()
                Nothing -> do 
                    logError hLogger " -- Wrong vkToken or group_id"
                    exitFailure                    
          getUpdatesVk handle hLogger dl = do  
            let sess = session dl
                ts = offset dl
            resEither <- try (httpLBS  $ eventBuildRequest sess conf ts)
            res' <- testException resEither handle hLogger
            res  <- messageOK res' conf hLogger  
            let answerMaybe = (decode $ getResponseBody res) :: Maybe Answer
            when (answerMaybe == Nothing) $ do
              logWarning hLogger " -- requesting new values key and ts"
              (initSession handle) handle hLogger
            let answer = fromJust answerMaybe
            when (a_ts answer == Nothing) $ do
              logWarning hLogger " -- requesting new values key and ts"
              (initSession handle) handle hLogger
            let upds = fromJust $ a_updates answer
                newts = fromJust $ a_ts answer
            logDebug hLogger (" List of Updates received = \n " ++ (show upds))
            return dl {updates = map VK upds, offset = newts}
          copyMessagesVk handle hLogger dl = do 
            mapM_ copyMessage $ forCopy upds conf dict
            return dl
            where
              upds = updates dl
              dict = dictionary dl
              copyMessage (VK x) =  do
                let event = getVk_ItemMessage x
                logInfo hLogger (" to user " ++ (show $ m_from_id event))
                httpLBS  $ echoBuildRequest conf event
          sendMessagesWithKbVK handle hLogger dl = do
            mapM_ sendMessageWithKb $ forKb upds
            return dl
            where
              upds = updates dl
              dict = dictionary dl
              sendMessageWithKb (VK x) = do
                let event = getVk_ItemMessage x
                logInfo hLogger (" to user " ++ (show $ m_from_id event))
                httpLBS  $ kbBuildRequest conf dict event
          sendMessagesWithHelpVK handle hLogger dl = do
            mapM_ sendMessageWithHelp $ forHelp upds
            return dl
            where
              upds = updates dl
              sendMessageWithHelp (VK x) = do
                let event = getVk_ItemMessage x
                logInfo hLogger (" to user " ++ (show $ m_from_id event))
                httpLBS  $ helpBuildRequest conf event
          newDictVK handle hLogger dl = do
            let upds = updates dl
                dict = dictionary dl
                newdict = execState ( mapM_ changeMapInt
                                     $ getUserAndNumRep
                                     $ listUpdWithKey upds) dict
            return dl {dictionary = newdict}
                                          

