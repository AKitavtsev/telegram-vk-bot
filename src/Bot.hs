module Bot
    where

import DataVK
import Drop
import Config

-- import MapR

data Handle = Handle
    { conf :: Config
    , initSession :: Handle -> IO Session
    }
    
