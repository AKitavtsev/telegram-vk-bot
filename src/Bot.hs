{-# LANGUAGE RankNTypes #-}

module Bot
  ( Bot.Handle(..)
  , DataLoop(..)
  , Upd (..)
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

import Dictionary
import Session

import Services.Logger as SL

data Handle a =
  Handle 
    { getUpdates :: Upd a => Bot.Handle a -> SL.Handle -> DataLoop a -> IO (DataLoop a)
    , copyMessages :: Upd a => SL.Handle -> DataLoop a -> IO (DataLoop a)
    , sendMessagesWithKb :: Upd a => SL.Handle -> DataLoop a -> IO (DataLoop a) 
    , sendMessagesWithHelp :: Upd a => SL.Handle -> DataLoop a -> IO (DataLoop a)
    }
  
data DataLoop a =
  DataLoop
    { session :: Session
    , updates :: [a]
    , dictionary :: MapInt
    , offset :: String
    }

class Upd a where
  -- user_id :: a -> Integer
  getUserAndNumRep :: [a] -> [(Int, Int)]
  listUpdWithKey :: [a] -> [a]
  
loopBot :: Upd a => Bot.Handle a -> SL.Handle -> DataLoop a -> IO ()
loopBot botHandle hLogger dl = do
  newDl <-
    getUpdates botHandle botHandle hLogger dl >>= copyMessages botHandle hLogger >>=
    sendMessagesWithKb botHandle hLogger >>=
    sendMessagesWithHelp botHandle hLogger
  loopBot botHandle hLogger (newDict newDl)

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

newDict :: Upd a => DataLoop a -> DataLoop a     
newDict dl = dl {dictionary = dict'}
  where
    upds = updates dl
    dict = dictionary dl
    dict' = execState
            (mapM_ changeMapInt $ getUserAndNumRep $ listUpdWithKey upds) dict

