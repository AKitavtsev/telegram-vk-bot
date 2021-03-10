module Log
    where

data LoggLevel = DEBUG | INFO | WARN | ERROR |UNDEF deriving (Show, Eq, Ord)

data Handle = Handle
    { debugM :: LoggLevel -> String -> String -> IO ()
    , infoM  :: LoggLevel -> String -> String -> IO ()
    , warnM  :: LoggLevel -> String -> String -> IO ()
    , errorM :: LoggLevel -> String -> String -> IO ()
    , undefM :: LoggLevel -> String -> String -> IO ()
    }
    
