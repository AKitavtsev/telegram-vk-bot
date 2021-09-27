
module Bot.Telegram
  ( newHandle
  ) where

import Control.Exception

import Data.Aeson
import Network.HTTP.Simple

import Bot
import Bot.Telegram.Internal
import Bot.Telegram.Types

import Config
import Services.Logger as SL


newHandle :: Config -> IO (Bot.Handle Update)
newHandle conf = do
  return $
    Bot.Handle
      { getUpdates = getUpdatesTl
      , copyMessages = copyMessagesTl
      , sendMessagesWithKb = sendMessagesWithKbTl
      , sendMessagesWithHelp = sendMessagesWithHelpTl
      }
  where
    getUpdatesTl handleTl hLogger dl = do
      let offs = offset dl
      resEither <- try (httpLBS $ eventBuildRequest conf offs)
      res' <- testException resEither hLogger
      res <- messageOK res' hLogger
      let upds = listUpd (decode $ getResponseBody res)
      logDebug hLogger (" List of Updates received = \n " ++ show upds)
      return dl {updates = upds, offset = newoffs upds}
    copyMessagesTl hLogger dl = do
      mapM_ copyMessage $ forCopy upds conf dict
      return dl
      where
        upds = updates dl
        dict = dictionary dl
        copyMessage x = do
          logInfo hLogger (" to user " ++ show (usId x))
          httpLBS $ echoBuildRequest conf x
    sendMessagesWithKbTl hLogger dl = do
      mapM_ sendMessageWithKb $ forKb upds
      return dl
      where
        upds = updates dl
        dict = dictionary dl
        sendMessageWithKb x = do
          logInfo hLogger (" to user " ++ show (usId x))
          httpLBS $ kbBuildRequest conf dict x
    sendMessagesWithHelpTl hLogger dl = do
      mapM_ sendMessageWithHelp $ forHelp upds
      return dl
      where
        upds = updates dl
        sendMessageWithHelp x = do
          logInfo hLogger (" to user " ++ show (usId x))
          httpLBS $ helpBuildRequest conf x
