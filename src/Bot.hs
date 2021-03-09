module Bot
    where

import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Simple
import Control.Monad.State

import Drop
import Config
import Log
import MapR
import DataTelegram
import DataVK

data Handle = Handle
    { config         :: Config
    , handlerLog     :: Log.Handle
    , initSession    :: Bot.Handle -> IO ()
    , getUpdates     :: Bot.Handle -> Session -> String -> IO ([UPD], String)
    , forCopy        :: [UPD] -> Bot.Handle -> MapInt -> [UPD]
    , forHelp        :: [UPD] -> [UPD]
    , forKb          :: [UPD] -> [UPD]
    , listUpdWithKey :: [UPD] -> [UPD]
    , copyMessage    :: UPD -> IO (Response LBC.ByteString)
    , sendMessWithKeyboard :: MapInt -> UPD -> IO (Response LBC.ByteString)
    , helpMessage    :: UPD -> IO (Response LBC.ByteString)
    , getUserAndNumRep :: [UPD] -> [(Int, Int)]
    }
    
data UPD = Tl Update | VK Event deriving (Show)
    
loopBot :: Bot.Handle -> Session -> MapInt -> String -> IO ()
loopBot handle sess dict ts = do
    let logLevel = сonfigLogg $ config handle 
        debM     = (debugM $ handlerLog handle) logLevel
        titleM   = "-- Bot.loopBot"
    debM titleM ("ts = " ++ ts ++ "  dict = " ++ show dict)
    (upds, newts) <- (getUpdates handle) handle sess ts    
    mapM_ (copyMessage handle) ((forCopy handle) upds handle dict)
    mapM_ ((sendMessWithKeyboard handle) dict) ((forKb handle) upds)
    mapM_ (helpMessage handle) ((forHelp handle) upds)
    let lp = (listUpdWithKey handle) upds
        newdict = execState (mapChangeMapInt $ (getUserAndNumRep handle) lp) dict
    loopBot handle sess newdict newts

    
    
    

    
