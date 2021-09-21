module Bot.VK
  ( newHandle
  , initSession
  ) where

import Control.Exception
import Control.Monad.State
import Data.Aeson
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple
import System.Exit

import qualified Data.Map as M

import Bot
import Bot.VK.Internal
import Bot.VK.Types
import Services.Config
import Services.Logger as SL

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
      res' <- testException resEither hLogger
      res <- messageOK res' hLogger
      let answer =
            fromMaybe
              (Answer Nothing Nothing Nothing)
              ((decode $ getResponseBody res) :: Maybe Answer)
          newts = fromMaybe "" (a_ts answer)
          upds = fromMaybe [] (a_updates answer)
      when (newts == "") $ do
        logWarning hLogger " -- requesting new values key and ts"
        loopBot  (Bot.VK.initSession handleVK hLogger conf) handleVK hLogger (DataLoop (Session "" "" "0") [] M.empty "0") 
      logDebug hLogger (" getUpdates Exit -- DataLoop = \n " ++ show dl)
      return dl {updates = upds, offset = newts}
    copyMessagesVk hLogger dl = do
      logDebug hLogger (" copyMessages In -- DataLoop = \n " ++ show dl)    
      mapM_ copyMessage $ forCopy upds conf dict
      logDebug hLogger (" copyMessages Exit -- DataLoop = \n " ++ show dl)
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
      logDebug hLogger (" sendMessagesWithKb  In-- DataLoop = \n " ++ show dl)    
      mapM_ sendMessageWithKb $ forKb upds
      logDebug hLogger (" sendMessagesWithKb  Exit -- DataLoop = \n " ++ show dl)    
      return dl
      where
        upds = updates dl
        dict = dictionary dl
        sendMessageWithKb x = do
          let event = getVkItemMessage x
          logInfo hLogger (" to user " ++ show (m_from_id event))
          logDebug hLogger (" sendMessagesWithKb -- List of Updates received = \n " ++ show upds) 
          httpLBS $ kbBuildRequest conf dict event
    sendMessagesWithHelpVK hLogger dl = do
      logDebug hLogger (" sendMessagesWithHelp In -- DataLoop = \n " ++ show dl)    
      mapM_ sendMessageWithHelp $ forHelp upds
      logDebug hLogger (" sendMessagesWithHelp Exit -- DataLoop = \n " ++ show dl)    
      return dl
      where
        upds = updates dl
        sendMessageWithHelp x = do
          let event = getVkItemMessage x
          logInfo hLogger (" to user " ++ show (m_from_id event))
          logDebug hLogger (" sendMessagesWithHelp -- List of Updates received = \n " ++ show upds) 
          httpLBS $ helpBuildRequest conf event

initSession :: Bot.Handle Event -> SL.Handle -> Config -> IO ()
initSession  handleVK hLogger conf = do
  resEither <- try (httpLBS $ initBuildRequest conf)
  res <- testException resEither hLogger
  let rsc = getResponseStatusCode res
  when (rsc /= 200) $ do
    logError hLogger ("-- status code of response " ++ show rsc)
    exitFailure
  let sessionJson = getResponseBody res
  let sessionR = decode sessionJson :: Maybe SessionResponse
  case sessionR of
    (Just (VKResponse x)) -> do
      logDebug hLogger ("-- initialized session with parameters:\n" ++ show x)
      loopBot (pure ()) handleVK hLogger (DataLoop x [] M.empty $ ts x)
      return ()
    Nothing -> do
      logError hLogger " -- Wrong vkToken or groupId"
      exitFailure
