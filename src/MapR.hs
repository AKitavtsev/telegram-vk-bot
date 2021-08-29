module MapR
    (MapInt
    ,changeMapInt
    )
    where

import Control.Monad.State

import qualified Data.Map as M

type MapInt = M.Map Int Int

changeMapInt :: (Int, Int) -> State MapInt ()
changeMapInt (k, v) = do
    dict <- get
    let dict' = M.delete k dict
    put (M.insert k v dict')
    