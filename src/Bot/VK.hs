module Bot.VK
  ( newHandle
  , initSession
  ) where

import Control.Exception
import Control.Monad.State
import Data.Aeson
import Data.Maybe (fromMaybe, isNothing)
import Network.HTTP.Simple
import System.Exit

import qualified Data.Map as M

import Bot
import Bot.VK.Internal
import Bot.VK.Types
import Config
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
    getUpdatesVk botHandle hLogger dl = do
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
        initSession botHandle hLogger conf
      logDebug hLogger (" List of Updates received = \n " ++ show upds)
      return dl {updates = upds, offset = newts}
    copyMessagesVk hLogger dl = do
      mapM_ copyMessage $ forCopy upds conf dict
      return dl
      where
        upds = updates dl
        dict = dictionary dl
        copyMessage x = do
          let event = getVkItemMessage x
          logInfo hLogger (" to user " ++ show (m_from_id event))
          httpLBS $ echoBuildRequest conf event
    sendMessagesWithKbVK hLogger dl = do
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
      mapM_ sendMessageWithHelp $ forHelp upds
      return dl
      where
        upds = updates dl
        sendMessageWithHelp x = do
          let event = getVkItemMessage x
          logInfo hLogger (" to user " ++ show (m_from_id event))
          httpLBS $ helpBuildRequest conf event

initSession :: Upd a => Bot.Handle a -> SL.Handle -> Config -> IO ()
initSession botHandle hLogger conf = do
  resEither <- try (httpLBS $ initBuildRequest conf)
                 -- :: IO (Either SomeException (Response LBC.ByteString))
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
      loopBot botHandle hLogger (DataLoop x [] M.empty $ ts x)
      
      return ()
    Nothing -> do
      logError hLogger " -- Wrong vkToken or groupId"
      exitFailure
