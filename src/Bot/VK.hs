module Bot.VK
  ( newHandle
  , initSession
  , eventSetting
  ) where

import Control.Exception
import Control.Monad.State
import Data.Aeson
import Data.Maybe (fromMaybe, isJust)
import Network.HTTP.Simple

import qualified Data.Map as M

import Bot
import Bot.VK.Internal
import Bot.VK.Types
import Config
import Services.Logger as SL

import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy.Char8 as LBC

newHandle :: Config -> IO (Bot.Handle Event)
newHandle conf = do
  return $
    Bot.Handle
      { getUpdates = getUpdatesVk
      , copyMessages = copyMessagesVk
      , sendMessagesWithKb = sendMessagesWithKbVK
      , sendMessagesWithHelp = sendMessagesWithHelpVK
      }
  where
    getUpdatesVk handleVK hLogger dl = do
      logDebug hLogger (" getUpdates In -- DataLoop = \n " ++ show dl)    
      let sess = session dl
          offs = offset dl
      resEither <- try (httpLBS $ eventBuildRequest sess conf offs)
                   :: IO (Either SomeException (Response LBC.ByteString))
      case resEither of
        Left _ -> do
          exceptionHandling (pure ()) handleVK hLogger dl
          return dl
        Right res' -> do 
          res <- messageOK res' hLogger
          logDebug hLogger (" -- ResponseBody:" ++  (LBC.unpack $ getResponseBody res))
          let answer = fromMaybe
                       (Answer Nothing Nothing Nothing)
                       ((decode $ getResponseBody res) :: Maybe Answer)              
              newts = fromMaybe "0" (a_ts answer)
              upds = fromMaybe [] (a_updates answer)
          logDebug hLogger ("-- answer  "++ (show answer))           
          when (isJust (a_failed answer) || newts == "0") $ do
            logWarning hLogger " -- requesting new values key and ts"
            loopBot (Bot.VK.initSession handleVK hLogger conf (dictionary dl))
                    handleVK 
                    hLogger 
                    (DataLoop (Session "" "" "0") [] (dictionary dl) "0") 
          return dl {updates = upds, offset = newts}
    copyMessagesVk hLogger dl = do
      logDebug hLogger (" copyMessages In -- DataLoop = \n " ++ show dl)    
      mapM_ copyMessage $ forCopy upds conf dict
      return dl
      where
        upds = updates dl
        dict = dictionary dl
        copyMessage x = do
          let event = getVkItemMessage x
          logInfo hLogger (" to user " ++ show (m_from_id event))
          logDebug hLogger (" copyMessages -- List of Updates received = \n " ++ show upds) 
          httpLBS $ echoBuildRequest conf event
    sendMessagesWithKbVK hLogger dl = do
      logDebug hLogger (" sendMessagesWithKb  In -- DataLoop = \n " ++ show dl)    
      mapM_ sendMessageWithKb $ forKb upds
      return dl
      where
        upds = updates dl
        dict = dictionary dl
        sendMessageWithKb x = do
          let event = getVkItemMessage x
          logInfo hLogger (" to user " ++ show (m_from_id event))
          httpLBS $ kbBuildRequest conf dict event
    sendMessagesWithHelpVK hLogger dl = do
      logDebug hLogger (" sendMessagesWithHelp In -- DataLoop = \n " ++ show dl)    
      mapM_ sendMessageWithHelp $ forHelp upds
      return dl
      where
        upds = updates dl
        sendMessageWithHelp x = do
          let event = getVkItemMessage x
          logInfo hLogger (" to user " ++ show (m_from_id event))
          httpLBS $ helpBuildRequest conf event

initSession :: Bot.Handle Event -> SL.Handle -> Config -> UserRepetition -> IO ()
initSession  handleVK hLogger conf dict = do
  resEither <- try (httpLBS $ initBuildRequest conf)
               :: IO (Either SomeException (Response LBC.ByteString))
  case resEither of
    Left _ -> 
          exceptionHandling 
            (initSession handleVK hLogger conf dict)
            handleVK
            hLogger
            (DataLoop (Session "" "" "0") [] dict "0")
    Right res -> do 
      let rsc = getResponseStatusCode res
      when (rsc /= 200) $ do
        logError hLogger ("-- status code of response " ++ show rsc)
        exitFailure
      let sessionJson = getResponseBody res
      let sessionR = decode sessionJson :: Maybe SessionResponse
      case sessionR of
        (Just (VKResponse x)) -> do
          logDebug hLogger ("-- initialized session with parameters:\n" ++ show x)
          loopBot (pure ()) handleVK hLogger (DataLoop x [] dict $ ts x)
          return ()
        Nothing -> do
          logError hLogger " -- Wrong vkToken or groupId"
          exitFailure

eventSetting :: SL.Handle -> Config -> IO ()
eventSetting hLogger conf = do
  resEither <- try (httpLBS $ setBuildRequest conf)
                   :: IO (Either SomeException (Response LBC.ByteString))
  case resEither of
    Left _ -> do 
      logError hLogger "-- Connection Failure"
      exitFailure 
    Right res -> do 
      let rsc = getResponseStatusCode res
      when (rsc /= 200) $ do
        logError hLogger ("-- status code of response " ++ show rsc)
        exitFailure
      return ()