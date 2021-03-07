module Bot
    where


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
    , getUpdates     :: Bot.Handle -> Session -> IO [UPD]
    , forCopy        :: [UPD] -> Bot.Handle -> MapInt -> [UPD]
    , forHelp        :: [UPD] -> [UPD]
    , forKb          :: [UPD] -> [UPD]
    , listUpdWithKey :: [UPD] -> [UPD]
    }
    
data UPD = Tl Update | VK Event deriving (Show)
    
loopBot :: Bot.Handle -> Session -> MapInt -> IO ()
loopBot handle sess dict = do
    let conf    = config handle
    (debugM $ handlerLog handle) (сonfigLogg conf) "-- LoopBot" 
                                   ("ts = " ++ (ts  sess) ++ "  dict = " ++ show dict)
    upds <- (getUpdates handle) handle sess
    
    print upds
    print $ (forCopy handle) upds handle dict
    print $ (forHelp handle) upds
    print $ (forKb handle) upds
    print $ (listUpdWithKey handle) upds

    
    return ()
    

    
