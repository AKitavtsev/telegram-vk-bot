module Bot.Telegram 
  ( newHandle
  ) where

import Control.Exception
-- import Control.Monad.State
import Data.Aeson
import Network.HTTP.Simple

-- import qualified Data.Map as M

import Bot
import Bot.Telegram.Internal
import Bot.Telegram.Types
-- import Dictionary
import Services.Config
import Services.Logger as SL
-- import Session
--
newHandle :: Config -> IO (Bot.Handle Update)
newHandle conf = do
  return $
    Bot.Handle
      { getUpdates = getUpdatesTl
      , copyMessages = copyMessagesTl
      , sendMessagesWithKb = sendMessagesWithKbTl
      , sendMessagesWithHelp = sendMessagesWithHelpTl
      -- , newDict = newDictTl
      }
  where
    getUpdatesTl botHandle hLogger dl = do
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
    -- newDictTl dl = dl {dictionary = dict'}
      -- where
        -- upds = updates dl
        -- dict = dictionary dl
        -- dict' =
          -- execState
            -- (mapM_ changeMapInt $ getUserAndNumRep $ listUpdWithKey upds)
            -- dict

