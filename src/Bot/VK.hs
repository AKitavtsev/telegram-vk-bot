{-# LANGUAGE OverloadedStrings #-}

module Bot.VK
  ( newHandle
  ) where

import Control.Exception
import Control.Monad.State
import Data.Aeson
import Data.Maybe (isNothing)
import Network.HTTP.Simple
import System.Exit

import qualified Data.Map as M

import Bot
import Bot.VK.Data
import Bot.VK.Internal
import Drop
import MapR
import Servises.Config
import Servises.Logger

--
newHandle :: Config -> IO Bot.Handle
newHandle conf = do
  return $
    Bot.Handle
      { initSession = initSessionVK
      , getUpdates = getUpdatesVk
      , copyMessages = copyMessagesVk
      , sendMessagesWithKb = sendMessagesWithKbVK
      , sendMessagesWithHelp = sendMessagesWithHelpVK
      , newDict = newDictVK
      }
  where
    initSessionVK botHandle hLogger = do
      resEither <- try (httpLBS $ initBuildRequest conf)
                 -- :: IO (Either SomeException (Response LBC.ByteString))
      res <- testException resEither botHandle hLogger
      let rsc = getResponseStatusCode res
      when (rsc /= 200) $ do
        logError hLogger ("-- status code of response " ++ show rsc)
        exitFailure
      let sessionJson = getResponseBody res
      let sessionR = decode sessionJson :: Maybe SessionResponse
      case sessionR of
        (Just (VKResponse x)) -> do
          logDebug
            hLogger
            ("-- initialized session with parameters:\n" ++ show x)
          loopBot botHandle hLogger (DataLoop x [] M.empty $ ts x)
          return ()
        Nothing -> do
          logError hLogger " -- Wrong vkToken or group_id"
          exitFailure
    getUpdatesVk botHandle hLogger dl = do
      let sess = session dl
          offs = offset dl
      resEither <- try (httpLBS $ eventBuildRequest sess conf offs)
      res' <- testException resEither botHandle hLogger
      res <- messageOK res' hLogger
      let answerMaybe = (decode $ getResponseBody res) :: Maybe Answer
      when (isNothing answerMaybe) $ do
        logWarning hLogger " -- requesting new values key and ts"
        initSession botHandle botHandle hLogger
      let answer = fromJust answerMaybe
      when (isNothing (a_ts answer)) $ do
        logWarning hLogger " -- requesting new values key and ts"
        initSession botHandle botHandle hLogger
      let upds = fromJust $ a_updates answer
          newts = fromJust $ a_ts answer
      logDebug hLogger (" List of Updates received = \n " ++ show upds)
      return dl {updates = map VK upds, offset = newts}
    copyMessagesVk hLogger dl = do
      mapM_ copyMessage $ forCopy upds conf dict
      return dl
      where
        upds = updates dl
        dict = dictionary dl
        copyMessage ~(VK x) = do
          let event = getVkItemMessage x
          logInfo hLogger (" to user " ++ (show $ m_from_id event))
          httpLBS $ echoBuildRequest conf event
    sendMessagesWithKbVK hLogger dl = do
      mapM_ sendMessageWithKb $ forKb upds
      return dl
      where
        upds = updates dl
        dict = dictionary dl
        sendMessageWithKb ~(VK x) = do
          let event = getVkItemMessage x
          logInfo hLogger (" to user " ++ (show $ m_from_id event))
          httpLBS $ kbBuildRequest conf dict event
    sendMessagesWithHelpVK hLogger dl = do
      mapM_ sendMessageWithHelp $ forHelp upds
      return dl
      where
        upds = updates dl
        sendMessageWithHelp ~(VK x) = do
          let event = getVkItemMessage x
          logInfo hLogger (" to user " ++ (show $ m_from_id event))
          httpLBS $ helpBuildRequest conf event
    newDictVK dl = do
      let upds = updates dl
          dict = dictionary dl
          newdict =
            execState
              (mapM_ changeMapInt $ getUserAndNumRep $ listUpdWithKey upds)
              dict
      return dl {dictionary = newdict}
