module Bot
    where

-- import DataVK
import Drop
import Config
import Log

-- import MapR

data Handle = Handle
    { conf :: Config
    , handlerLog :: Log.Handle
    , initSession :: Bot.Handle -> IO Session
    }
    
