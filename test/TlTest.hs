{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  TlTest where

import Test.Hspec
import Control.Monad.State

import qualified Data.Map as M

import Config
import DataTelegram
import MapR
import Telegram


tlTest :: IO ()
tlTest = hspec $ do
  let upMessage = Update 146878330
                (Just (Message 261
                      (Just (User 484189456 False "Андрей")) (Just "222"))) 
                Nothing
      upCallbackQuery = Update 146878330
                Nothing
                (Just (CallbackQuery "2079577882558908621" 
                      (User 484189457 False "Андрей")  (Just "333")))      
  describe "newoffs" $ do
    it "returns offset for next request Update" $
      newoffs [upMessage] `shouldBe` 146878331
  describe "usId" $ do
    it "returns user_id from Message" $
      usId upMessage `shouldBe` 484189456
    it "returns user_id from CallbackQuery" $
      usId upCallbackQuery `shouldBe` 484189457 
  describe "cbData" $ do
    it "returns data from CallbackQuery" $
      cbData upCallbackQuery `shouldBe` "333"   


