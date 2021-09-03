module Bot
  ( Bot.Handle(..)
  , UPD(..)
  , DataLoop(..)
  , loopBot
  , messageOK
  , testException
  , fromJust
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import System.Exit (exitFailure)

import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Simple

import Bot.Telegram.Types
import Bot.VK.Types
import Dictionary
import Session

import Services.Logger as SL


data Handle =
  Handle
    { initSession :: Bot.Handle -> SL.Handle -> IO ()
    , getUpdates :: Bot.Handle -> SL.Handle -> DataLoop -> IO DataLoop
    , copyMessages :: SL.Handle -> DataLoop -> IO DataLoop
    , sendMessagesWithKb :: SL.Handle -> DataLoop -> IO DataLoop
    , sendMessagesWithHelp :: SL.Handle -> DataLoop -> IO DataLoop
    , newDict :: DataLoop -> IO DataLoop
    }

data UPD
  = Tl Update
  | VK Event
  deriving (Show, Eq)

data DataLoop =
  DataLoop
    { session :: Session
    , updates :: [UPD]
    , dictionary :: MapInt
    , offset :: String
    }

loopBot :: Bot.Handle -> SL.Handle -> DataLoop -> IO ()
loopBot botHandle hLogger dl =
  getUpdates botHandle botHandle hLogger dl >>= copyMessages botHandle hLogger >>=
  sendMessagesWithKb botHandle hLogger >>=
  sendMessagesWithHelp botHandle hLogger >>=
  newDict botHandle >>=
  loopBot botHandle hLogger

messageOK ::
     Response LBC.ByteString -> SL.Handle -> IO (Response LBC.ByteString)
messageOK res hLogger = do
  let rsc = getResponseStatusCode res
  when (rsc /= 200) $ do
    logError hLogger ("-- status code of response " ++ show rsc)
    exitFailure
  return res

testException ::
     Either SomeException (Response LBC.ByteString)
  -> Bot.Handle
  -> SL.Handle
  -> IO (Response LBC.ByteString)
testException rese botHandle hLogger = do
  case rese of
    Right val -> return val
    Left _ -> do
      logError
        hLogger
        "-- Connection Failure -- Trying to initialize the session"
      threadDelay 25000000
      initSession botHandle botHandle hLogger
      httpLBS defaultRequest

fromJust :: Maybe a -> a
fromJust ~(Just x) = x
