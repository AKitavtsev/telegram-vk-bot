{-# LANGUAGE TemplateHaskell #-}

module Bot
    ( Bot.Handle (..)
    , UPD (..)
    , DataLoop (..)
    , loopBot
    , messageOK
    , testException
    )
        where
import Control.Exception (SomeException)
import System.Exit (exitFailure)
import Control.Concurrent (threadDelay)

import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Simple
import Control.Monad.State

import Drop
import MapR
import Bot.Telegram.Data
import Bot.VK.Data

import Servises.Logger as SL
import qualified Servises.Config as SC

data Handle = Handle
    { initSession          :: Bot.Handle -> SL.Handle -> IO ()
    , getUpdates           :: Bot.Handle -> SL.Handle -> DataLoop -> IO DataLoop
    , copyMessages         :: Bot.Handle -> SL.Handle -> DataLoop -> IO DataLoop
    , sendMessagesWithKb   :: Bot.Handle -> SL.Handle -> DataLoop -> IO DataLoop
    , sendMessagesWithHelp :: Bot.Handle -> SL.Handle -> DataLoop -> IO DataLoop
    , newDict              :: Bot.Handle -> SL.Handle -> DataLoop -> IO DataLoop
    }

data UPD = Tl Update| VK Event  deriving (Show, Eq)

data DataLoop = DataLoop { session :: Session
                         , updates :: [UPD]
                         , dictionary :: MapInt
                         , offset :: String
                         }
    
loopBot :: Bot.Handle -> SL.Handle -> DataLoop -> IO ()
loopBot handle hLogger dl = return dl        >>=
                 (getUpdates handle) handle hLogger >>=
                 (copyMessages handle) handle hLogger >>=
                 (sendMessagesWithKb handle) handle hLogger >>= 
                 (sendMessagesWithHelp handle) handle hLogger >>=
                 (newDict handle) handle hLogger >>=
                 loopBot handle hLogger
                
messageOK :: (Response LBC.ByteString)
          -> SC.Config
          -> SL.Handle
          -> IO (Response LBC.ByteString)        
messageOK res conf hLogger = do
            let rsc = getResponseStatusCode res
            when ( not (rsc == 200)) $ do
              logError hLogger ("-- status code of response " ++ show rsc)
              exitFailure
            return res
                                        
testException :: (Either SomeException (Response LBC.ByteString))
              ->  Bot.Handle
              ->  SL.Handle
              ->  IO (Response LBC.ByteString)
testException rese handle hLogger = do
    case rese of
        Right val -> return val
        Left ex   -> do 
            logError hLogger
                "-- Connection Failure -- Trying to initialize the session"
            threadDelay 25000000
            (initSession handle) handle hLogger
            httpLBS defaultRequest            

