module Services.Impl.StdOut
  ( newHandle
  ) where

import qualified Services.Config as SC
import qualified Services.Logger as SL

import Control.Monad (when)

newHandle :: SC.Config -> IO SL.Handle
newHandle config = do
  return $ SL.Handle {SL.logPriority = logPriority}
  where
    logPriority prio msg = do
      when (prio >= SC.levelLog config) $ do putStrLn (show prio ++ msg)
