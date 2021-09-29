{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Bot
  ( Bot.Handle(..)
  , DataLoop(..)
  , Upd(..)
  , Session(..)
  , UserRepetition
  , newDict
  , loopBot
  , messageOK
  , exceptionHandling
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Simple
import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M

import Services.Logger as SL

data Handle a =
  Handle
    { getUpdates :: Upd a =>
                      Bot.Handle a -> SL.Handle -> DataLoop a -> IO (DataLoop a)
    , copyMessages :: Upd a =>
                        SL.Handle -> DataLoop a -> IO (DataLoop a)
    , sendMessagesWithKb :: Upd a =>
                              SL.Handle -> DataLoop a -> IO (DataLoop a)
    , sendMessagesWithHelp :: Upd a =>
                                SL.Handle -> DataLoop a -> IO (DataLoop a)
    }

data Session =
  Session
    { key :: String
    , server :: String
    , ts :: String
    }
  deriving (FromJSON, Show, Generic, Eq)

type UserRepetition = M.Map Int Int

data DataLoop a =
  DataLoop
    { session :: Session
    , updates :: [a]
    , dictionary :: UserRepetition
    , offset :: String
    }
  deriving (Show, Eq)

class Upd a where
  usId :: a -> Int
  mesId :: a -> Int
  txt :: a -> String
  getUserAndNumRep :: [a] -> [(Int, Int)]
  listUpdWithKey :: [a] -> [a]

loopBot :: Upd a => IO () -> Bot.Handle a -> SL.Handle -> DataLoop a -> IO ()
loopBot botInit botHandle hLogger dl = do
  botInit
  newDl <-
    getUpdates botHandle botHandle hLogger dl >>= 
    copyMessages botHandle hLogger >>=
    sendMessagesWithKb botHandle hLogger >>=
    sendMessagesWithHelp botHandle hLogger
  loopBot (pure ()) botHandle hLogger (newDict newDl)

messageOK ::
     Response LBC.ByteString -> SL.Handle -> IO (Response LBC.ByteString)
messageOK res hLogger = do
  let rsc = getResponseStatusCode res
  when (rsc /= 200) $ do
    logError hLogger ("-- status code of response " ++ show rsc)
    exitFailure
  return res

exceptionHandling :: Upd a => IO () -> Bot.Handle a -> SL.Handle -> DataLoop a -> IO ()
exceptionHandling  initBot botHandle hLogger dl = do
  logError hLogger "-- Connection Failure -- Wait and try again"
  threadDelay 25000000
  loopBot initBot botHandle hLogger dl
  return ()
      
newDict :: Upd a => DataLoop a -> DataLoop a
newDict dl = dl {dictionary = dict}
  where
    dict = updateDictionary (dictionary dl) $ getUserAndNumRep (listUpdWithKey $ updates dl)
    updateDictionary d [] = d
    updateDictionary d ((k, v):xs) = updateDictionary (M.insert k v d) xs
