module Services.Impl.StdOut
  ( newHandle
  ) where
import Config
import qualified Services.Logger as SL

import Control.Monad (when)

newHandle :: Config -> IO SL.Handle
newHandle config = do
  return $ SL.Handle {SL.logPriority = logPriority}
  where
    logPriority prio msg = do
      when (prio >= levelLog config) $ do putStrLn (show prio ++ msg)
