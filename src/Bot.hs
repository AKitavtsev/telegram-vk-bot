module Bot
    -- ( Bot.Handle
    -- , Tl, VK, UPD
    -- , loopBot
    -- )
        where

import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Simple
import Control.Monad.State

import Drop
import Config
import Log as Log
import MapR
import Telegram.Data
import VK.Data

data Handle = Handle
    { config               :: Config
    , handlerLog           :: Log.Handle
    , initSession          :: Bot.Handle -> IO ()
    , getUpdates           :: Bot.Handle -> Session -> String -> IO ([UPD], String)
    , copyMessages         :: [UPD] -> MapInt -> IO ()
    , sendMessagesWithKb   :: [UPD] -> MapInt -> IO ()
    , sendMessagesWithHelp :: [UPD] -> IO ()
    , newDict              :: [UPD] -> MapInt -> MapInt
    }


data UPD = Tl Update| VK Event  deriving (Show, Eq)
    
loopBot :: Bot.Handle -> Session -> MapInt -> String -> IO ()
loopBot handle sess dict ts = do
    let logLevel = сonfigLogg $ config handle 
        debM     = (debugM $ handlerLog handle) logLevel
        titleM   = "-- Bot.loopBot"
    debM titleM ("ts = " ++ ts ++ "  dict = " ++ show dict)
    (upds, newts) <- (getUpdates handle) handle sess ts    
    (copyMessages handle) upds dict
    (sendMessagesWithKb handle) upds dict
    (sendMessagesWithHelp handle) upds
    loopBot handle sess ((newDict handle) upds dict) newts
