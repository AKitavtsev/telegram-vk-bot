module Bot
    where


import Drop
import Config
import Log
import MapR
import DataTelegram
import DataVK


data Handle = Handle
    { config      :: Config
    , handlerLog  :: Log.Handle
    , initSession :: Bot.Handle -> IO Session
    , getUpdates  :: Log.Handle -> Config -> Session -> IO UPD
    }
    
data UPD = Tl [Update] | VK [Event] deriving (Show)
    
loopBot :: Bot.Handle -> Session -> MapInt -> IO ()
loopBot handle sess dict = do
    let conf    = config handle
        handlog = handlerLog handle
    print "!!!!!!!!!!!!!!!!!!!!!!!!!!"
    (debugM handlog) (сonfigLogg conf) "-- LoopBot" 
                                   ("ts = " ++ (ts  sess) ++ "  dict = " ++ show dict)
    upds <- (getUpdates handle) handlog conf sess
    print upds
 
    
    
    return ()
    

    
