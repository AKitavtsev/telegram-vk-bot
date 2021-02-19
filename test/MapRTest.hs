{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  MapRTest where

import Test.Hspec
import Control.Monad.State

import qualified Data.Map as M

import MapR


mapRTest :: IO ()
mapRTest = hspec $ do
  describe "changeMapInt" $ do
    it "add in empty" $ 
      runState (changeMapInt (5, 5))  M.empty `shouldBe` ((), M.singleton 5 5)
    it "add in not empty" $ 
      runState (changeMapInt (2, 22))  (M.fromList [(1,2)]) `shouldBe` ((), (M.fromList [(1,2), (2,22)]))
    it "replace " $ 
      runState (changeMapInt (1, 22))  (M.fromList [(1,2)]) `shouldBe` ((), (M.fromList [(1,22)]))
