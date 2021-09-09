{-# LANGUAGE RankNTypes #-}

module Bot
  ( Bot.Handle(..)
  -- , UPD(..)
  , DataLoop(..)
  , loopBot
  , messageOK
  , testException
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import System.Exit (exitFailure)

import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Simple

-- import Bot.Telegram.Types
-- import Bot.VK.Types
import Dictionary
import Session

import Services.Logger as SL

data Handle a =
  Handle 
    { getUpdates :: Bot.Handle a -> SL.Handle -> DataLoop a -> IO (DataLoop a)
    , copyMessages :: SL.Handle -> DataLoop a -> IO (DataLoop a)
    , sendMessagesWithKb :: SL.Handle -> DataLoop a -> IO (DataLoop a) 
    , sendMessagesWithHelp :: SL.Handle -> DataLoop a -> IO (DataLoop a)
    , newDict :: DataLoop a -> DataLoop a
    }

-- class Upd a where
  -- user_id :: a -> Integer
  
data DataLoop a =
  DataLoop
    { session :: Session
    , updates :: [a]
    , dictionary :: MapInt
    , offset :: String
    }

loopBot :: Bot.Handle a -> SL.Handle -> DataLoop a -> IO ()
loopBot botHandle hLogger dl = do
  newDl <-
    getUpdates botHandle botHandle hLogger dl >>= copyMessages botHandle hLogger >>=
    sendMessagesWithKb botHandle hLogger >>=
    sendMessagesWithHelp botHandle hLogger
  loopBot botHandle hLogger (newDict botHandle newDl)

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
  -- -> Bot.Handle
  -> SL.Handle
  -> IO (Response LBC.ByteString)
testException rese  
-- botHandle 
              hLogger = do
  case rese of
    Right val -> return val
    Left _ -> do
      logError
        hLogger
        "-- Connection Failure -- Trying to initialize the session"
      threadDelay 25000000
      -- initSession botHandle hLogger
      _ <- exitFailure
      httpLBS defaultRequest
--
