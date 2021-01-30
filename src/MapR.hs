module MapR
    (MapInt
    ,changeMapInt

    ) 
    where

import Control.Monad.State

import qualified Data.Map as M

type MapInt = M.Map Int Int

-- findWithDefault :: Ord k => a -> k -> Map k a -> a

-- findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'

changeMapInt :: Int -> Int -> State MapInt ()
changeMapInt k v = do
    dict <- get
    let dict' = M.delete k dict
    put (M.insert k v dict')

