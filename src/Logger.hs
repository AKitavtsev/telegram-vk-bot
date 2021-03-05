module Logger where

import Control.Monad (when)

import Log

newHandle :: IO Log.Handle
newHandle = do
    return $ Log.Handle
        { Log.debugM = \logg pref str  -> do
            when (logg <= DEBUG) $ do 
                putStrLn ("DEBUG  " ++ pref ++ "   " ++ str)
                return ()
         , Log.infoM = \logg pref str  -> do
            when (logg <= INFO) $ do
                putStrLn ("INFO   " ++ pref ++ "   " ++ str)
                return ()
         , Log.warnM = \logg pref str  -> do
            when (logg <= WARN) $ do
                putStrLn ("WARN   " ++ pref ++ "   " ++ str)
                return ()
         , Log.errorM = \logg pref str  -> do
            when (logg  <= ERROR) $ do
                putStrLn ("ERROR  " ++ pref ++ "   " ++ str)
                return ()
         , Log.undefM = \logg pref str  -> do
                putStrLn ("UNDEF  " ++ pref ++ str)
                return ()
         }
         
         