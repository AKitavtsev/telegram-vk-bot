module Services.Impl.StdOut
  ( newHandle
  ) where

--
import Services.Types (Priority(..))

import qualified Services.Config as SC
import qualified Services.Logger as SL

import Control.Monad (when)

newHandle :: SC.Config -> IO SL.Handle
newHandle config = do
  return $ SL.Handle {SL.config = config, SL.logPriority = logPriority}
  where
    logPriority prio msg = do
      let lev =
            case config of
              (SC.LogConfig x) -> x
              _ -> ERROR
      when (prio >= lev) $ do putStrLn (show prio ++ msg)
